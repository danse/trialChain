### trialChain

Build, test, run

    stack build
    stack test
    stack exec trialChain-exe

#### Example

With bash and curl, we can send a simple transaction to get its id back

    $ d="{\"sources\":[[3,\"3\"]],\"destinations\":[2],\"amount\":1.0}"
    $ curl -H "Content-Type:  application/json" -d $d localhost:8080/broadcast

Copying the received transaction id into an `i` variable we can
retrieve the transaction

    $ i=158-249-45-39-226-178-125-121-14-139-91-242-177-22-39-103
    $ curl localhost:8080/retrieve/$i

The server stores transactions in a "data" file in the directory where
it is run from
