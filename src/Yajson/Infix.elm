module Yajson.Infix exposing ((=>))

{-|


# Rocket operator

@docs (=>)

-}


{-| The rocket operator can be used to create tuples
with a friendly syntax. See [here](Yajson#Json).

    ("key" => "value") == ( "key", "value" )

-}
(=>) : a -> b -> ( a, b )
(=>) =
    (,)
