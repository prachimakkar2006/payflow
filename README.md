# PayFlow — Payment Orchestration Engine

PayFlow is a console-based payment orchestration simulator built in Haskell. It models payment lifecycles, processes payment events safely, and routes transactions across simulated payment gateways.

## Features

- **Payment Lifecycle:** Models payment state transitions and rejects invalid transitions.
- **Idempotent Event Processing:** Prevents duplicate webhook events from being applied more than once.
- **Rule-Based Routing:** Uses custom routing rules to determine eligible payment gateways.
- **Gateway Ranking:** Ranks gateways using success-rate statistics with Laplace smoothing.
- **Fallback Routing:** Attempts another eligible gateway when a gateway fails.
- **Property Checks:** Includes checks for lifecycle correctness, duplicate events, and routing rules.

## Tech Stack

- Haskell
- GHC

## Run Locally

```bash
ghc Main.hs -o payflow
```

On Windows, run:

```powershell
.\payflow.exe
```

## Project Scope

This project is a local simulation of payment orchestration logic. It does not process real payments or connect to external payment gateways.