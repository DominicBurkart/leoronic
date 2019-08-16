from ..wrappers import *


def test_apply():
    assert apply(lambda x: x * x, 5) == 25


def test_apply_async():
    async_call = apply_async(lambda x: x * x, 5)
    r = async_call.get_record()
    assert handle_completed(r) == 25
    assert r.stdout == ""
    assert r.stderr == ""


def test_apply_async_print():
    async_print = apply_async(lambda x: print(x), "value of x printed to stdout")
    r = async_print.get_record()
    assert handle_completed(r) == None
    assert r.stderr == ""
    assert r.stdout == "value of x printed to stdout"


def test_blocking_functions():
    def f1(x: int, y: int) -> int:
        return x * y

    def f2(x: int) -> int:
        return 2 * x

    tuple_inputs = [(x, y) for x in range(2) for y in range(2)]
    int_inputs = range(2)

    starmap_local_output = [f1(*inps) for inps in tuple_inputs]
    map_local_output = [f2(inp) for inp in int_inputs]

    starmap_output = list(starmap(f1, tuple_inputs))
    map_output = map(f2, int_inputs)
    imap_output = imap(f2, int_inputs)
    imap_unordered_output = imap_unordered(f2, int_inputs)

    assert starmap_output == starmap_local_output
    assert map_output == map_local_output
    # assert list(imap_output) == map_local_output todo
    # assert all(v in map_local_output for v in imap_unordered_output) todo


def test_async_functions():
    def f1(x: int, y: int):
        return x * y

    def f2(x: int):
        return x + 10

    input_tups = [(x, y) for x in range(2) for y in range(2)]
    input_ints = range(2)
    starmap_output = [a.get() for a in starmap_async(f1, input_tups)]
    map_output = [a.get() for a in map_async(f2, input_ints)]

    local_tups_output = [f1(*inps) for inps in input_tups]
    local_ints_output = [f2(inp) for inp in input_ints]

    assert starmap_output == local_tups_output
    assert map_output == local_ints_output
