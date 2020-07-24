
See the [FA1.2 Lorentz Tutorial](https://assets.tqtezos.com/docs/token-contracts/fa12/3-fa12-lorentz/)
for more detail.

# Setup

The "./stack" file is a pointer to a compatible version of `stack`: `1.9.3`.

It must be configured for `morley` versions that require
older versions of `stack`.

Run `./stack build` to build the CLI tool(s).


# Really quick start

Originated copies of `ExecLambda`:

- Carthagenet: [`KT1PCtQTdgD44WsYgTzAUUztMcrDmPiSuSV1`](https://better-call.dev/carthagenet/KT1PCtQTdgD44WsYgTzAUUztMcrDmPiSuSV1/code)
- Mainnet (Carthage): [`KT1CPuTzwC7h7uLXd5WQmpMFso1HxrLBUtpE`](https://better-call.dev/mainnet/KT1CPuTzwC7h7uLXd5WQmpMFso1HxrLBUtpE/code)

You'll need to configure the CLI tool with an address of an `ExecLambda`
contract, see `Originating the contract`.

```bash
EXECLAMBDA_ADDRESS="KT1PCtQTdgD44WsYgTzAUUztMcrDmPiSuSV1"

alias tz-view="./lorentz-contract-view.rb \
  'tezos-client -A rpcalpha.tzbeta.net -P 443 -S' \
  $EXECLAMBDA_ADDRESS $BOB_ADDRESS"
```


We'll also need a contract address to query:

```bash
FA12_ADDRESS="KT18apu7iDnqnUeXdMv3ZVjs81DTPWK6f1Me"
```

The client can list all of the (named) entrypoints and their types:

```bash
❯❯❯ tezos-client get contract entrypoints for $FA12_ADDRESS

Entrypoints for contract KT18apu7iDnqnUeXdMv3ZVjs81DTPWK6f1Me: 
  default: (or (or (or (pair %transfer (address :from) (pair (address :to) (nat :value)))
                       (pair %approve (address :spender) (nat :value)))
                   (or (pair %getAllowance (pair (address :owner) (address :spender)) (contract nat))
                       (or (pair %getBalance (address :owner) (contract nat))
                           (pair %getTotalSupply unit (contract nat)))))
               (or (or (bool %setPause)
                       (or (address %setAdministrator) (pair %getAdministrator unit (contract address))))
                   (or (pair %mint (address :to) (nat :value))
                       (or (pair %burn (address :from) (nat :value))
                           (pair %getMetadata
                              (list nat)
                              (contract (list (pair nat (pair string (pair string (pair nat (map string string))))))))))))
  transfer: (pair (address :from) (pair (address :to) (nat :value)))
  setPause: bool
  setAdministrator: address
  mint: (pair (address :to) (nat :value))
  getTotalSupply: (pair unit (contract nat))
  getMetadata: (pair (list nat)
                     (contract (list (pair nat (pair string (pair string (pair nat (map string string))))))))
  getBalance: (pair (address :owner) (contract nat))
  getAllowance: (pair (pair (address :owner) (address :spender)) (contract nat))
  getAdministrator: (pair unit (contract address))
  burn: (pair (address :from) (nat :value))
  approve: (pair (address :spender) (nat :value))
```

E.g. `getBalance` accepts an `address` and
a callback contract accepting a `nat`.

Finally, run `tz-view` with:
- The contract address
- The entrypoint name
- The view parameter

```bash
❯❯❯ tz-view $FA12_ADDRESS 'getTotalSupply' 'Unit'

Running:
...

Result:
5
```


# The CLI

```bash
❯❯❯ ./stack exec -- lorentz-contract-view --help
Lorentz tools

Usage: lorentz-contract-view COMMAND
  ExecLambda contract parameter generation helper

Available options:
  -h,--help                Show this help text

Available commands:
  print                    Dump the ExecLambda contract in form of Michelson
                           code
  print-const-view         Dump the ConstView contract (specialized to 'nat') in
                           form of Michelson code
  view-to-void             Make an ExecLambda parameter to access a contract's
                           View parameter off-chain using FAILWITH, i.e. like a
                           Void_ parameter.

You can use help for specific COMMAND
EXAMPLE:
  lorentz-contract-view COMMAND --help
```


# Originating the contract

## Printing the contract

The print command only has optional arguments:

```bash
❯❯❯ ./stack exec -- lorentz-contract-view print --help
Usage: lorentz-contract-view print [-o|--output FILEPATH] [--oneline]
  Dump the ExecLambda contract in form of Michelson code

Available options:
  -h,--help                Show this help text
  -o,--output FILEPATH     File to use as output. If not specified, stdout is
                           used.
  --oneline                Force single line output
  -h,--help                Show this help text
```

Here's what it looks like:

```bash
❯❯❯ ./stack exec -- lorentz-contract-view print
parameter (lambda unit
                  (pair (list operation)
                        unit));
storage unit;
code { CAR;
       UNIT;
       EXEC };
```

## Running the origination

```bash
❯❯❯ tezos-client --wait none originate contract ExecLambda \
  transferring 0 from $ALICE_ADDRESS running \
 "$(cat contracts/exec_lambda.tz | tr -d '\n')" \
  --burn-cap 0.303

Waiting for the node to be bootstrapped before injection...
Current head: BMXNZDPdnSNL (timestamp: 2020-07-24T20:51:54-00:00, validation: 2020-07-24T20:52:26-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 11120 units (will add 100 for safety)
Estimated storage: 303 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooLS9prYwHvf8ppZt5qCgxuwbjXqvi4YBDLoRRK45vswdc5Qnxd'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooLS9prYwHvf8ppZt5qCgxuwbjXqvi4YBDLoRRK45vswdc5Qnxd to be included --confirmations 30 --branch BLig2b8tmWMDL1CLhmUbnvCfjr4MMHLJbdfZGKmBzhu1e1iD4gw
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001397
    Expected counter: 624003
    Gas limit: 11220
    Storage limit: 323 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001397
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,289) ... +ꜩ0.001397
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (lambda unit (pair (list operation) unit)) ;
          storage unit ;
          code { CAR ; UNIT ; EXEC } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1PCtQTdgD44WsYgTzAUUztMcrDmPiSuSV1
        Storage size: 46 bytes
        Paid storage size diff: 46 bytes
        Consumed gas: 11120
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.046
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1PCtQTdgD44WsYgTzAUUztMcrDmPiSuSV1 originated.
Contract memorized as ExecLambda.
```

Make a `bash` alias for the address:

```bash
EXECLAMBDA_ADDRESS="KT1PCtQTdgD44WsYgTzAUUztMcrDmPiSuSV1"
```


# Interacting with the contract

## View to Void

`View` entrpoints have the form: `(a, ContractRef r)`

When a `View` entrypoint is called, the first element of the tuple
is used to run the entrypoint and the results are sent
to the `ContractRef` callback address.

This requires a contract that accepts the requisite result parameter type.

While this is fit for on-chain (i.e. inter-contract) passing results,
it's not immediately amenable to access off-chain.

For off-chain access, `Void_` entrypoints can be used:
viz. `View` entrypoints that use `FAILWITH` instead of calling a contract.
These failures can be seen using the Tezos client with `--dry-run`.

However, while `Void_` entrypoints don't require any gas to call,
they require at least `O(1)` gas to be added to the blockchain
(a problem that remains if some sort of combination `View` + `Void_`
entrypoints are used).

Instead of using `Void_`, we can call `View` entrypoints off-chain
with a one-time cost of `0.303 tez` for _all possible entrypoints and `View`'s_
using the `ExecLambda` contract.

How? We craft a lambda that:
- Originates a sink contract that receives a parameter of the specific type and runs `FAILWITH` on it
- Calls itself with the resulting address (through `APPLY`): the sink contract will already be originated
- Calls the `View` entrypoint, passing it the sink contract's address as the callback contract address


### Originating a Contract with a View

#### Printing a Contract with a View

`ConstView` is a contract with a single baked-in value that we can `View`:

```bash
❯❯❯ ./stack exec -- lorentz-contract-view print-const-view --help
Usage: lorentz-contract-view print-const-view --constToView NATURAL
                                              [-o|--output FILEPATH] [--oneline]
  Dump the ConstView contract (specialized to 'nat') in form of Michelson code

Available options:
  -h,--help                Show this help text
  --constToView NATURAL    Natural number representing constToView.
  -o,--output FILEPATH     File to use as output. If not specified, stdout is
                           used.
  --oneline                Force single line output
  -h,--help                Show this help text
```

It only needs one parameter: `constToView`

```bash
❯❯❯ ./stack exec -- lorentz-contract-view print-const-view --constToView 42
parameter (pair unit
                (contract nat));
storage unit;
code { DUP;
       CAR;
       DIP { CDR };
       DUP;
       CAR;
       DIP { CDR };
       DIP { DIP { DUP };
             SWAP };
       PAIR;
       DROP;
       PUSH nat 42;
       DIP { AMOUNT };
       TRANSFER_TOKENS;
       NIL operation;
       SWAP;
       CONS;
       PAIR };
```


#### Originating a Contract with a View

```bash
❯❯❯ alpha-client --wait none originate contract ViewNat42 \
  transferring 0 from $ALICE_ADDRESS running \
  "$(./stack exec -- lorentz-contract-view print-const-view --constToView 42 \
  --oneline)" --burn-cap 0.368

Waiting for the node to be bootstrapped before injection...
Current head: BMdRB3nBxduy (timestamp: 2019-12-12T20:27:26-00:00, validation: 2019-12-12T20:27:38-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13087 units (will add 100 for safety)
Estimated storage: 368 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opFbcck3AZQAoLnh4YZZbEFTyMMsrmKQDZdmXuoZCkMfEse7oeM'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opFbcck3AZQAoLnh4YZZbEFTyMMsrmKQDZdmXuoZCkMfEse7oeM to be included --confirmations 30 --branch BMdRB3nBxduyEsP3aJdBdx8eFVwLSuUKTUBzdKtmmkhHtPfYCEo
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.001659
    Expected counter: 30720
    Gas limit: 13187
    Storage limit: 388 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.001659
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,64) ... +ꜩ0.001659
    Origination:
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      Credit: ꜩ0
      Script:
        { parameter (pair unit (contract nat)) ;
          storage unit ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 DUP ;
                 CAR ;
                 DIP { CDR } ;
                 DIP { DIP { DUP } ; SWAP } ;
                 PAIR ;
                 DROP ;
                 PUSH nat 42 ;
                 DIP { AMOUNT } ;
                 TRANSFER_TOKENS ;
                 NIL operation ;
                 SWAP ;
                 CONS ;
                 PAIR } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1Rc9HR6dqBf6MxN6cCJjDZ6PidaxvT93U8
        Storage size: 111 bytes
        Paid storage size diff: 111 bytes
        Consumed gas: 13087
        Balance updates:
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.111
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.257

New contract KT1Rc9HR6dqBf6MxN6cCJjDZ6PidaxvT93U8 originated.
Contract memorized as ViewNat42.
```

Make a `bash` alias for the contract:

```bash
VIEW42_ADDRESS="KT1Rc9HR6dqBf6MxN6cCJjDZ6PidaxvT93U8"
```


### Off-chain accessing a View

#### Generating the ExecLambda Parameter

The `view-to-void` command generates parameters for the `ExecLambda`
contract that let us call `View` entrypoints like `Void` entrypoints.

```bash
❯❯❯ ./stack exec -- lorentz-contract-view view-to-void --help
Usage: lorentz-contract-view view-to-void --execLambda ADDRESS
                                          --parameterTypeType Michelson Type
                                          --callbackTypeType Michelson Type
                                          --viewContract ADDRESS
                                          [--viewEntrypoint STRING]
                                          --viewParameter STRING
  Make an ExecLambda parameter to access a contract's View parameter off-chain
  using FAILWITH, i.e. like a Void_ parameter.

Available options:
  -h,--help                Show this help text
  --execLambda ADDRESS     The ExecLambda contract's Address
  --parameterTypeType Michelson Type
                           The Michelson Type of parameterType
  --callbackTypeType Michelson Type
                           The Michelson Type of callbackType
  --viewContract ADDRESS   The contract whose view parameter you want to call
  --viewEntrypoint STRING  The entrypoint name of the view you want to call, or
                           empty for default
  --viewParameter STRING   The parameter value for the 'View' entrypoint you
                           want to call
  -h,--help                Show this help text
```

```bash
❯❯❯ ./stack exec -- lorentz-contract-view view-to-void \
  --execLambda $EXECLAMBDA_ADDRESS \
  --parameterType "unit" \
  --callbackType "nat" \
  --viewContract $VIEW42_ADDRESS \
  --viewParameter "Unit"

{ PUSH mutez 0; NONE key_hash; CREATE_CONTRACT{ parameter nat;storage unit;code { FAILWITH }; }; DIP { DIP { LAMBDA (pair address unit) (pair address unit)       { CAR;CONTRACT nat;IF_NONE { PUSH string "Not 'Tc 'CNat";FAILWITH }        {  };PUSH unit Unit;PAIR;DIP { PUSH address "KT1Rc9HR6dqBf6MxN6cCJjDZ6PidaxvT93U8";CONTRACT (pair unit (contract nat));IF_NONE { PUSH string "Not 'TPair 'TUnit ('TContract ('Tc 'CNat))";FAILWITH }        {  };PUSH mutez 0 };TRANSFER_TOKENS;DIP { NIL operation };CONS;DIP { UNIT };PAIR } };APPLY;DIP { PUSH address "KT1NFUsGvAomSSNnKjps8RL1EjGKfWQmM4iw";CONTRACT (lambda unit (pair (list operation) unit));IF_NONE { PUSH string "Not 'TLambda 'TUnit ('TPair ('TList 'TOperation) 'TUnit)";FAILWITH }        {  };PUSH mutez 0 };TRANSFER_TOKENS;DIP { NIL operation };CONS }; CONS; DIP { UNIT }; PAIR }
```


#### Calling ExecLambda

```bash
alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $EXECLAMBDA_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-view view-to-void \
    --execLambda $EXECLAMBDA_ADDRESS \
    --parameterType "unit" \
    --callbackType "nat" \
    --viewContract $VIEW42_ADDRESS \
    --viewParameter "Unit" | sed \
    's/(pair address unit) (pair address unit)/(pair address unit) (pair (list operation) unit)/')"
    --dry-run

alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $EXECLAMBDA_ADDRESS \
    --arg "$(./stack exec -- lorentz-contract-view view-to-void \
    --execLambda $EXECLAMBDA_ADDRESS \
    --parameterType "unit" \
    --callbackType "nat" \
    --viewContract $VIEW42_ADDRESS \
    --viewParameter "Unit" | sed \
    's/(pair address unit) (pair address unit)/(pair address unit) (pair (list operation) unit)/')" \
    --dry-run

Waiting for the node to be bootstrapped before injection...
Current head: BLAB39m27ZdX (timestamp: 2019-12-12T20:51:40-00:00, validation: 2019-12-12T20:52:34-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0
    Expected counter: 30721
    Gas limit: 800000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1NFUsGvAomSSNnKjps8RL1EjGKfWQmM4iw
      Parameter: { PUSH mutez 0 ;
                   NONE key_hash ;
                   CREATE_CONTRACT { parameter nat ; storage unit ; code { FAILWITH } } ;
                   DIP { DIP { LAMBDA
                                 (pair address unit)
                                 (pair (list operation) unit)
                                 { CAR ;
                                   CONTRACT nat ;
                                   IF_NONE { PUSH string "Not 'Tc 'CNat" ; FAILWITH } {} ;
                                   PUSH unit Unit ;
                                   PAIR ;
                                   DIP { PUSH address "KT1Rc9HR6dqBf6MxN6cCJjDZ6PidaxvT93U8" ;
                                         CONTRACT (pair unit (contract nat)) ;
                                         IF_NONE
                                           { PUSH string "Not 'TPair 'TUnit ('TContract ('Tc 'CNat))" ; FAILWITH }
                                           {} ;
                                         PUSH mutez 0 } ;
                                   TRANSFER_TOKENS ;
                                   DIP { NIL operation } ;
                                   CONS ;
                                   DIP { UNIT } ;
                                   PAIR } } ;
                         APPLY ;
                         DIP { PUSH address "KT1NFUsGvAomSSNnKjps8RL1EjGKfWQmM4iw" ;
                               CONTRACT (lambda unit (pair (list operation) unit)) ;
                               IF_NONE
                                 { PUSH string "Not 'TLambda 'TUnit ('TPair ('TList 'TOperation) 'TUnit)" ;
                                   FAILWITH }
                                 {} ;
                               PUSH mutez 0 } ;
                         TRANSFER_TOKENS ;
                         DIP { NIL operation } ;
                         CONS } ;
                   CONS ;
                   DIP { UNIT } ;
                   PAIR }
      This transaction was BACKTRACKED, its expected effects (as follow) were NOT applied.
      Updated storage: Unit
      Storage size: 46 bytes
      Consumed gas: 27008
    Internal operations:
      Origination:
        From: KT1NFUsGvAomSSNnKjps8RL1EjGKfWQmM4iw
        Credit: ꜩ0
        Script:
          { parameter nat ; storage unit ; code { FAILWITH } }
          Initial storage: Unit
          No delegate for this contract
          This origination was BACKTRACKED, its expected effects (as follow) were NOT applied.
          Originated contracts:
            KT1EZFJXsPvje2hcdiP41ioadR14mBbys8Ay
          Storage size: 32 bytes
          Paid storage size diff: 32 bytes
          Consumed gas: 10696
          Balance updates:
            tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.032
            tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.257
      Transaction:
        Amount: ꜩ0
        From: KT1NFUsGvAomSSNnKjps8RL1EjGKfWQmM4iw
        To: KT1NFUsGvAomSSNnKjps8RL1EjGKfWQmM4iw
        Parameter: { PUSH address 0x01418713a754e2b796f6581577c8fdc442b935edf900 ;
                     PAIR ;
                     { CAR ;
                       CONTRACT nat ;
                       IF_NONE { PUSH string "Not 'Tc 'CNat" ; FAILWITH } {} ;
                       PUSH unit Unit ;
                       PAIR ;
                       DIP { PUSH address 0x01babcaf043d2f2da252d3394899be247307f9e8cc00 ;
                             CONTRACT (pair unit (contract nat)) ;
                             IF_NONE
                               { PUSH string "Not 'TPair 'TUnit ('TContract ('Tc 'CNat))" ; FAILWITH }
                               {} ;
                             PUSH mutez 0 } ;
                       TRANSFER_TOKENS ;
                       DIP { NIL operation } ;
                       CONS ;
                       DIP { UNIT } ;
                       PAIR } }
        This transaction was BACKTRACKED, its expected effects (as follow) were NOT applied.
        Updated storage: Unit
        Storage size: 46 bytes
        Consumed gas: 35744
      Transaction:
        Amount: ꜩ0
        From: KT1NFUsGvAomSSNnKjps8RL1EjGKfWQmM4iw
        To: KT1Rc9HR6dqBf6MxN6cCJjDZ6PidaxvT93U8
        Parameter: (Pair Unit 0x01418713a754e2b796f6581577c8fdc442b935edf900)
        This transaction was BACKTRACKED, its expected effects (as follow) were NOT applied.
        Updated storage: Unit
        Storage size: 111 bytes
        Consumed gas: 13865
      Transaction:
        Amount: ꜩ0
        From: KT1Rc9HR6dqBf6MxN6cCJjDZ6PidaxvT93U8
        To: KT1EZFJXsPvje2hcdiP41ioadR14mBbys8Ay
        Parameter: 42
        This operation FAILED.

Runtime error in contract KT1EZFJXsPvje2hcdiP41ioadR14mBbys8Ay:
  1: { parameter nat ; storage unit ; code { FAILWITH } }
At line 1 characters 40 to 48,
script reached FAILWITH instruction
with (Pair 42 Unit)
Fatal error:
  transfer simulation failed
```

NOTE: What's up with `sed 's/(pair address unit) (pair address unit)/(pair address unit) (pair (list operation) unit)/'`?
- morley appears to be mangling the stack type using `ZippedStack`
- The proper emitted instruction is: `LAMBDA (pair address unit) (pair (list operation) unit)`,
  but morley currently emits: `LAMBDA (pair address unit) (pair address unit)`

