{-# LANGUAGE LambdaCase #-}
-- | PayFlow: a pure-Haskell payment orchestration core.
--   1. Payment lifecycle as a total, pure state machine
--   2. A routing-rules DSL (rules are data, not code)
--   3. Success-rate-aware gateway ranking + fallback (smart routing)
--   4. Idempotent event processing (duplicate webhooks are harmless)
--   Only base + containers. No IO inside the core.
module Main (main) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy, foldl')
import Data.Ord (comparing, Down (..))
import Control.Monad (forM_)

----------------------------------------------------------------
-- 1. Domain types
----------------------------------------------------------------

newtype Paise = Paise Int deriving (Eq, Ord, Show)   -- never use Float for money

data Method  = UPI | Card | NetBanking deriving (Eq, Ord, Show, Enum, Bounded)
data Gateway = GwAlpha | GwBeta | GwGamma deriving (Eq, Ord, Show, Enum, Bounded)

data Payment = Payment
  { pId     :: String
  , pAmount :: Paise
  , pMethod :: Method
  } deriving Show

data PayState
  = Created
  | Authorized Gateway
  | Captured Gateway
  | Failed String
  | Refunded Gateway
  deriving (Eq, Show)

data Event
  = Authorize Gateway
  | Capture
  | Fail String
  | Refund
  deriving (Eq, Show)

data TransitionError = IllegalTransition PayState Event
  deriving (Eq, Show)

----------------------------------------------------------------
-- 2. State machine: pure, total, illegal moves are values not crashes
----------------------------------------------------------------

transition :: PayState -> Event -> Either TransitionError PayState
transition s e = case (s, e) of
  (Created,      Authorize g) -> Right (Authorized g)
  (Created,      Fail r)      -> Right (Failed r)
  (Authorized g, Capture)     -> Right (Captured g)
  (Authorized _, Fail r)      -> Right (Failed r)
  (Captured g,   Refund)      -> Right (Refunded g)
  _                           -> Left (IllegalTransition s e)

-- (isTerminal kept for extension)
isTerminal :: PayState -> Bool
isTerminal = \case Failed _ -> True; Refunded _ -> True; _ -> False

----------------------------------------------------------------
-- 3. Idempotent processing: each event carries a unique id (webhook id)
----------------------------------------------------------------

data Store = Store
  { states :: M.Map String PayState
  , seen   :: S.Set String          -- processed event ids
  } deriving Show

emptyStore :: Store
emptyStore = Store M.empty S.empty

data Outcome = Applied | Duplicate | Rejected TransitionError
  deriving (Eq, Show)

-- | Apply an event once. Replays of the same event id are no-ops.
apply :: String -> String -> Event -> Store -> (Outcome, Store)
apply eid pid ev st
  | eid `S.member` seen st = (Duplicate, st)
  | otherwise =
      let cur = M.findWithDefault Created pid (states st)
      in case transition cur ev of
           Left err -> (Rejected err, st)      -- rejected events are NOT marked seen
           Right s' -> (Applied, Store (M.insert pid s' (states st))
                                       (S.insert eid (seen st)))

----------------------------------------------------------------
-- 4. Routing DSL: rules are data, so they can be stored, shown, tested
----------------------------------------------------------------

data Cond
  = MethodIs Method
  | AmountAtLeast Int
  | AmountBelow Int
  | Not Cond
  | And Cond Cond
  | Or Cond Cond
  | Always
  deriving Show

data Rule = Cond :=> [Gateway]     -- condition => allowed gateways
infixr 1 :=>

eval :: Cond -> Payment -> Bool
eval c p = case c of
  MethodIs m      -> pMethod p == m
  AmountAtLeast n -> let Paise a = pAmount p in a >= n
  AmountBelow n   -> let Paise a = pAmount p in a < n
  Not x           -> not (eval x p)
  And x y         -> eval x p && eval y p
  Or x y          -> eval x p || eval y p
  Always          -> True

-- | First matching rule wins; no match means all gateways are eligible.
eligible :: [Rule] -> Payment -> [Gateway]
eligible rules p = case [gs | (c :=> gs) <- rules, eval c p] of
  (gs : _) -> gs
  []       -> [minBound .. maxBound]

----------------------------------------------------------------
-- 5. Smart routing: rank by smoothed success rate
----------------------------------------------------------------

data Stats = Stats { ok :: Int, total :: Int } deriving Show
type Health = M.Map Gateway Stats

-- Laplace smoothing so a gateway with 1/1 does not beat one with 950/1000.
score :: Stats -> Double
score (Stats o t) = (fromIntegral o + 1) / (fromIntegral t + 2)

rank :: Health -> [Gateway] -> [Gateway]
rank h = sortBy (comparing (Down . score . stat))
  where stat g = M.findWithDefault (Stats 0 0) g h

recordResult :: Gateway -> Bool -> Health -> Health
recordResult g success = M.insertWith add g (Stats (fromEnum success) 1)
  where add (Stats a b) (Stats c d) = Stats (a + c) (b + d)

-- | Try gateways in ranked order until one succeeds. The gateway call is
--   passed in as a function, so the whole thing stays pure and testable.
--   Returns the winning gateway (if any), the attempt log, updated health.
route :: [Rule] -> Health -> (Gateway -> Bool) -> Payment
      -> (Maybe Gateway, [(Gateway, Bool)], Health)
route rules h attempt p = go (rank h (eligible rules p)) [] h
  where
    go [] logAcc hh = (Nothing, reverse logAcc, hh)
    go (g : gs) logAcc hh =
      let r   = attempt g
          hh' = recordResult g r hh
      in if r then (Just g, reverse ((g, r) : logAcc), hh')
              else go gs ((g, r) : logAcc) hh'

----------------------------------------------------------------
-- 6. Tests (exhaustive over small spaces; no external libs)
----------------------------------------------------------------

allEvents :: [Event]
allEvents = [Authorize g | g <- [minBound ..]] ++ [Capture, Fail "x", Refund]

-- every event sequence up to length n
sequencesUpTo :: Int -> [[Event]]
sequencesUpTo 0 = [[]]
sequencesUpTo n = [] : [e : rest | e <- allEvents, rest <- sequencesUpTo (n - 1)]

runSeq :: [Event] -> Maybe PayState
runSeq = foldl' step (Just Created)
  where step (Just s) e = either (const Nothing) Just (transition s e)
        step Nothing  _ = Nothing

check :: String -> Bool -> IO ()
check name b = putStrLn ((if b then "  PASS  " else "  FAIL  ") ++ name)

main :: IO ()
main = do
  putStrLn "== Property checks =="
  let seqs = sequencesUpTo 4
  -- P1: terminal states absorb: no event is ever legal after Failed/Refunded
  check "terminal states are absorbing" $
    and [ all (\e -> transition s e /= Right s && either (const True) (const False) (transition s e))
              allEvents
        | s <- [Failed "x", Refunded GwAlpha] ]
  -- P2: Refunded is reachable only via Captured (money safety)
  check "refund only reachable after capture" $
    and [ case runSeq es of
            Just (Refunded _) -> Capture `elem` es
            _                 -> True
        | es <- seqs ]
  -- P3: gateway never changes between authorize and capture/refund
  check "gateway is preserved through the lifecycle" $
    and [ case (runSeq es, [g | Authorize g <- es]) of
            (Just (Captured g'), g : _) -> g == g'
            (Just (Refunded g'), g : _) -> g == g'
            _ -> True
        | es <- seqs ]
  -- P4: idempotency: replaying any applied event id changes nothing
  let (_, s1) = apply "e1" "p1" (Authorize GwAlpha) emptyStore
      (o2, s2) = apply "e1" "p1" (Authorize GwAlpha) s1
  check "duplicate webhook is a no-op" (o2 == Duplicate && states s1 == states s2)
  -- P5: rejected events don't poison the store
  let (o3, s3) = apply "e9" "p1" Refund s1
  check "illegal event rejected, state unchanged"
    (o3 == Rejected (IllegalTransition (Authorized GwAlpha) Refund) && states s3 == states s1)
  -- P6: smoothing: 1/1 must not outrank 950/1000
  let h0 = M.fromList [(GwAlpha, Stats 1 1), (GwBeta, Stats 950 1000)]
  check "smoothed ranking prefers proven gateway" (head (rank h0 [GwAlpha, GwBeta]) == GwBeta)
  -- P7: routing rules respect eligibility
  let rules = [ MethodIs UPI `And` AmountBelow 100000 :=> [GwBeta]
              , AmountAtLeast 1000000                 :=> [GwGamma, GwAlpha]
              , Always                                :=> [GwAlpha, GwBeta, GwGamma] ]
      pUpi = Payment "a" (Paise 5000) UPI
      pBig = Payment "b" (Paise 2000000) Card
  check "rule: small UPI goes only to GwBeta" (eligible rules pUpi == [GwBeta])
  check "rule: big payments exclude GwBeta" (GwBeta `notElem` eligible rules pBig)

  putStrLn "\n== Demo: fallback routing =="
  let health = M.fromList [(GwGamma, Stats 970 1000), (GwAlpha, Stats 900 1000)]
      alwaysDownGamma g = g /= GwGamma          -- simulate GwGamma outage
      (winner, attempts, health') = route rules health alwaysDownGamma pBig
  putStrLn ("  eligible+ranked attempts: " ++ show attempts)
  putStrLn ("  winner: " ++ show winner)
  putStrLn ("  GwGamma stats after outage: " ++ show (M.lookup GwGamma health'))

  putStrLn "\n== Demo: full lifecycle with duplicate webhooks =="
  let evs = [ ("w1","pay1",Authorize GwAlpha), ("w2","pay1",Capture)
            , ("w2","pay1",Capture)             -- duplicate delivery
            , ("w3","pay1",Refund), ("w4","pay1",Capture) ]  -- illegal
      final = foldl' (\(st, logs) (e, p, ev) ->
                        let (o, st') = apply e p ev st in (st', logs ++ [(e, o)]))
                     (emptyStore, []) evs
  forM_ (snd final) $ \(e, o) -> putStrLn ("  " ++ e ++ " -> " ++ show o)
  putStrLn ("  final: " ++ show (M.toList (states (fst final))))
