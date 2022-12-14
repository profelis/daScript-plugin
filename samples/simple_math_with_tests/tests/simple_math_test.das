require dastest/testing_boost
require src/simple_math


[test]
def test_sum ( t : T? )
    t |> equal(3, sum(1, 2))
    t |> equal(3.2, sum(1.0, 2.2))
