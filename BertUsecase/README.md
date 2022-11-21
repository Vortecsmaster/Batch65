# HandsOns
eMurgo Academy Live session excercises


# TestNet Execution - Guessing Game contract

Summary:
> This exercise is going to walkthrough in executing a smart contract on a testnet, by serializing the different requirements for the transaction construction and finally constructing and submitting your transaction for execution using Cardano-CLI.

## Part 1 Prepare the elements necessary Transaction construction 

What do we need?****

* A working Plutus Developer Environment Nix-Shell
* 1 paymentAddresses with some UTxO with ADA for providing Bounty Value.
* 1 paymentAddress with a UTxO with ADA **ONLY** for providing collateral. (Could be the same payment address)
* 1 paymentAddress for the counterpart that try to guess the secret and get the bounty.
* Validator Script (Serialized and JSON encoded)
* Datum (JSON encoded)
* Redeemer (JSON encoded)


## Part 2 Serializing and encoding 

###### STEP 1
    Clone or Pull the handsOn repository
    git clone  https://github.com/Vortecsmaster/HandsOns.git


###### STEP 2
    Run your NIX-SHELL

###### STEP 3
In the your GuessingGame folder execute 

    cabal repl

###### STEP 4
On the REPL, Evaluate the functions on the Deploy module (load it if its not loaded)

    writeUnit
    writeDatum
    writeRedeemer
    writeBadRedeemer
    writeGGValidator

This is going to create the corresponding encoded/serialized files for unit, datum, goodRedeeer, badRedeemer and onChain validator.

###### STEP 5 

