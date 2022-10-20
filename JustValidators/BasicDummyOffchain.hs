--OFF-CHAIN

dummy :: Contract w DummySchema Text ()
dummy = return () 

type DummySchema = Endpoint "mint" ()

endpoints :: Contract () DummySchema Text ()
endpoints = dummy
--mint' >> endpoints
--  where
--    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''DummySchema
mkKnownCurrencies []