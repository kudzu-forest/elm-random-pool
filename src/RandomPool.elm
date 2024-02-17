module RandomPool exposing
    ( RandomPool
    , singleton, fromList
    , insertWithRelativeWeight, insertListWithRelativeWeight
    , suck, suckFor, cycle
    , get, discard
    , count, totalWeight
    )

{-|


# Types

@docs RandomPool


# Creation

@docs singleton, fromList


# Addition

@docs insertWithRelativeWeight, insertListWithRelativeWeight


# Random Operation

@docs suck, suckFor, cycle


# Pump Operation

@docs get, discard


# Query

@docs count, totalWeight

-}

import Queue as Q exposing (Queue)
import Random
import RandomTree exposing (RandomTree, Weighted)
import WideFloat exposing (WideFloat)


{-| Represents pooled data which can be randomly chosen.
The data are devided into two parts, named _pump_ and _pond_. Contents in pump are already chosen but not yet used. Contents in pond are yet to be chosen. This division is for implementing behavior like "the same thing never happens continuously".
-}
type RandomPool a
    = RandomPool
        { pump : Queue (Weighted a)
        , pond : RandomTree a
        }


{-| Returns the head(the oldest element) of pump.
-}
get : RandomPool a -> Maybe a
get (RandomPool rp) =
    case Q.head rp.pump of
        Just e ->
            Just e.content

        Nothing ->
            Nothing


{-| Removes the head of pump. Nothing occurs if the pump is empty.
-}
discard : RandomPool a -> RandomPool a
discard (RandomPool rp) =
    RandomPool
        { pump = Q.dequeue rp.pump
        , pond = rp.pond
        }


{-| Random generator that generates RandomePool which consists of pump with one more element randomly chosen from the pond and pond with that element removed.
-}
suck : RandomPool a -> Random.Generator (RandomPool a)
suck (RandomPool rp) =
    RandomTree.take rp.pond
        |> Random.map
            (\( e, mtree ) ->
                case mtree of
                    Just tree ->
                        RandomPool
                            { pump = Q.enqueue e rp.pump
                            , pond = tree
                            }

                    Nothing ->
                        RandomPool rp
            )


{-| `suck` for arbitrary times.
-}
suckFor : Int -> RandomPool a -> Random.Generator (RandomPool a)
suckFor n rp =
    if n > 0 then
        suck rp
            |> Random.andThen (suckFor (n - 1))

    else
        Random.constant rp


{-| Get one new element from the pond into pump, and changes the weight of the head and release it to the pond.
-}
cycle : Float -> RandomPool a -> Random.Generator ( a, RandomPool a )
cycle weightFactor (RandomPool rp) =
    case Q.head rp.pump of
        Just e ->
            let
                updatedWeight =
                    WideFloat.multiplyFloat weightFactor e.weight

                updatedElement =
                    { weight = updatedWeight
                    , content = e.content
                    }
            in
            RandomTree.replace updatedElement rp.pond
                |> Random.map
                    (\( e2, pond ) ->
                        ( e.content
                        , RandomPool
                            { pump =
                                rp.pump
                                    |> Q.dequeue
                                    |> Q.enqueue e2
                            , pond = pond
                            }
                        )
                    )

        Nothing ->
            RandomTree.take rp.pond
                |> Random.map
                    (\( e, mtree ) ->
                        let
                            updatedWeight =
                                WideFloat.multiplyFloat
                                    weightFactor
                                    e.weight

                            updatedElement =
                                { weight = updatedWeight
                                , content = e.content
                                }
                        in
                        case mtree of
                            Just tree ->
                                ( e.content
                                , RandomPool
                                    { pump = Q.empty
                                    , pond =
                                        RandomTree.insert
                                            updatedElement
                                            tree
                                    }
                                )

                            Nothing ->
                                ( e.content
                                , RandomPool
                                    { pump = Q.empty
                                    , pond =
                                        RandomTree.singleton
                                            updatedElement
                                    }
                                )
                    )


{-| Pour one new element into the pond. The first parameter is the relative weight of the added element. Relative weight of `1` means that the new element has the same weight as the total of all other elements in the pond.
-}
insertWithRelativeWeight : Float -> a -> RandomPool a -> RandomPool a
insertWithRelativeWeight f a (RandomPool rp) =
    RandomPool
        { pump = rp.pump
        , pond =
            RandomTree.insertWithRelativeWeight f a rp.pond
        }


{-| Inserts arbitrary number of elements into the pond.
The first component of parameter list content is relative weight. (`1` means total weight before any of them are inserted.)
-}
insertListWithRelativeWeight : List ( Float, a ) -> RandomPool a -> RandomPool a
insertListWithRelativeWeight list (RandomPool rp) =
    RandomPool
        { pump = rp.pump
        , pond =
            RandomTree.insertListWithRelativeWeight list rp.pond
        }


{-| Creates new `randompool` with only one element in the pond and empty pump.
-}
singleton : a -> RandomPool a
singleton a =
    RandomPool
        { pump = Q.empty
        , pond =
            RandomTree.singleton
                { weight = WideFloat.fromFloat 1
                , content = a
                }
        }


{-| Creates new `randompool` with all the element in the list poured into pond and empty pump.
-}
fromList : ( Float, a ) -> List ( Float, a ) -> RandomPool a
fromList ( hw, hc ) list =
    let
        eList =
            List.map
                (\( w, c ) ->
                    { weight = WideFloat.fromFloat w
                    , content = c
                    }
                )
                list
    in
    RandomPool
        { pump = Q.empty
        , pond =
            RandomTree.fromList
                { weight = WideFloat.fromFloat hw
                , content = hc
                }
                eList
        }


{-| Returnes how many contents in the pool.
-}
count : RandomPool a -> Int
count (RandomPool rp) =
    Q.length rp.pump
        + RandomTree.count rp.pond


{-| Returnes the sum of weight in the pool represented as `WideFloat`.
-}
totalWeight : RandomPool a -> WideFloat
totalWeight (RandomPool rp) =
    WideFloat.add
        (RandomTree.totalWeight rp.pond)
        (Q.fold
            (.weight >> WideFloat.add)
            WideFloat.zero
            rp.pump
        )
