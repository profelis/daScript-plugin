module simple_math shared


[sideeffects]
def sum(a, b : int) : int
    return a + b

[sideeffects]
def sum(a, b : float)
    return a + b
