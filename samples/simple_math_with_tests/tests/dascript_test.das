require dastest/testing_boost


[test]
def test_dascript_math ( t : T? )
    t |> equal(3, 1 + 2)
    t |> equal(3.2, 1.0 + 2.2)
