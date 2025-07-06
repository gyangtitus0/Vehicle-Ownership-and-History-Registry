# Vehicle Ownership & History Registry

A Clarity smart contract for tracking vehicle ownership, history, and preventing odometer fraud on the Stacks blockchain.

## Overview

This contract provides a decentralized system for:
- Registering vehicles with their VIN, manufacturer, model, and year
- Tracking ownership transfers with timestamps
- Recording odometer readings to prevent fraud
- Documenting service history and accidents
- Verifying vehicle history for potential buyers

## Contract Functions

### Read-Only Functions

- `get-contract-owner`: Returns the contract owner's principal
- `get-vehicle-details`: Returns complete details for a vehicle by VIN
- `get-vehicle-history-events`: Returns historical events for a vehicle
- `get-vehicle-owner`: Returns the current owner of a vehicle
- `get-ownership-period`: Returns ownership period for a specific owner
- `verify-odometer`: Verifies if a claimed odometer reading is valid

### Public Functions

- `register-vehicle`: Register a new vehicle with its details
- `transfer-ownership`: Transfer vehicle ownership to a new owner
- `update-odometer`: Update the current odometer reading
- `record-service`: Record a service event with description
- `record-accident`: Record an accident with description

## Usage Examples

### Registering a Vehicle

```clarity
(contract-call? .ownership register-vehicle "1HGCM82633A123456" "Honda" "Accord" u2020 u0)
```

### Transferring Ownership

```clarity
(contract-call? .ownership transfer-ownership "1HGCM82633A123456" 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u15000)
```

### Updating Odometer

```clarity
(contract-call? .ownership update-odometer "1HGCM82633A123456" u20000)
```

### Recording Service

```clarity
(contract-call? .ownership record-service "1HGCM82633A123456" u25000 "Oil change and brake service")
```

### Recording Accident

```clarity
(contract-call? .ownership record-accident "1HGCM82633A123456" u30000 "Minor fender bender, front bumper replaced")
```

## Error Codes

- `u100`: Not authorized
- `u101`: Vehicle already exists
- `u102`: Vehicle not found
- `u103`: Not the vehicle owner
- `u104`: Invalid odometer reading (lower than current)
- `u105`: Invalid transfer

## Security Considerations

- Only the current owner can transfer ownership or update vehicle information
- Odometer readings can only increase, never decrease
- All historical events are permanently recorded on the blockchain