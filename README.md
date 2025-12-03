# Delegation Service

Service to construct and submit Cardano delegation transactions.

## Overview

The Delegation Service is a REST API service built with Haskell that enables users to build and submit Cardano delegation transactions. It supports delegating to stake pools (SPOs), DReps (Delegated Representatives), or both simultaneously.

## Features

- **Pool Delegation**: Delegate stake to a Cardano stake pool using a pool ID
- **DRep Delegation**: Delegate voting power to a DRep using a DRep hash
- **Combined Delegation**: Delegate to both a stake pool and DRep in a single transaction
- **Transaction Building**: Construct unsigned delegation transactions
- **Transaction Submission**: Submit signed transactions to the Cardano network
- **Swagger UI**: Interactive API documentation available at `/swagger-ui`
- **Health Checks**: Built-in health and readiness probes

## API Endpoints

### Build Transaction
```
POST /build-tx
```
Builds a delegation transaction and returns it as a hex-encoded CBOR.

**Request Body:**
```json
{
  "action": {
    "PoolDelegation": {
      "poolId": "pool1..."
    }
  },
  "userAddresses": {
    "stakeAddresses": ["stake1..."]
  }
}
```

**Delegation Actions:**
- `PoolDelegation`: `{"poolId": "string"}`
- `DRepDelegation`: `{"dRepHash": "string"}`
- `PoolAndDRepDelegation`: `{"poolId": "string", "dRepHash": "string"}`

**Response:** Hex-encoded CBOR transaction string

### Submit Transaction
```
POST /submit-tx
```
Submits a signed transaction to the Cardano network.

**Request Body:**
```json
{
  "tx_unsigned": {...},
  "tx_wit": "..."
}
```

**Response:** Transaction ID (hex string)

### Health Checks
- `GET /health`: Health check endpoint
- `GET /ready`: Readiness check endpoint

## Configuration

The service requires an Atlas Cardano configuration file. By default, it looks for `config/config_atlas.json`, but this can be overridden with the `ATLAS_CORE_CONFIG` environment variable.

### Environment Variables

- `ATLAS_CORE_CONFIG`: Path to Atlas configuration file (default: `config/config_atlas.json`)
- `PORT`: Server port (default: `8082`)
- Basic Auth credentials can be configured via environment variables (see `mg-servant-utils` documentation)

## Building

### Prerequisites

- GHC 9.6.6 or later
- Cabal 3.0 or later
- Cardano dependencies (libsodium, secp256k1, BLST)
- Atlas Cardano SDK

### Build Steps

1. Install dependencies:
```bash
cabal update
cabal build --only-dependencies
```

2. Build the project:
```bash
cabal build
```

3. Install the executable:
```bash
cabal install delegation-service
```

## Running

### Local Development

```bash
cabal run delegation-service
```

The service will:
- Start on `http://0.0.0.0:8082` (or the port specified by `PORT`)
- Generate Swagger documentation at `./docs/swagger/delegation-swagger-api.json`
- Make Swagger UI available at `http://localhost:8082/swagger-ui`

### Docker

Build the Docker image:
```bash
docker build -t delegation-service .
```

Run the container:
```bash
docker run -p 8082:8082 \
  -v $(pwd)/config:/app/config \
  -e ATLAS_CORE_CONFIG=/app/config/config_atlas.json \
  delegation-service
```

## Project Structure

```
DelegationService/
├── src/
│   ├── app/              # Application entry point and REST API
│   │   ├── Main.hs       # Main application
│   │   ├── RestAPI.hs    # API definitions
│   │   └── DelegationAppMonad.hs
│   ├── lib/              # Library code
│   │   ├── TxBuilding/   # Transaction building logic
│   │   │   ├── Operations.hs
│   │   │   ├── Interactions.hs
│   │   │   └── Exceptions.hs
│   │   └── Utils/        # Utility modules
│   └── test/             # Test suite
├── config/               # Configuration files
├── docs/                 # Documentation
│   └── swagger/          # Generated Swagger documentation
├── Dockerfile            # Docker build configuration
└── DelegationService.cabal
```

## Dependencies

### Key Libraries
- `atlas-cardano`: Cardano blockchain integration
- `servant`: Type-safe web API framework
- `tx-building-core`: Transaction building utilities
- `mg-servant-utils`: Servant utilities (auth, CORS, health checks)

## License

Apache-2.0



## API Documentation

Interactive API documentation is available via Swagger UI when the service is running:
- Swagger UI: `http://localhost:8082/swagger-ui`
- Swagger JSON: `http://localhost:8082/swagger-api.json`

The Swagger specification is also generated at `./docs/swagger/delegation-swagger-api.json` when the service starts.

