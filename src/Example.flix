def test01(): Unit \ IO = {
    let machine =
        mapN(10, Direction.Right, _ -> "s") `sequence`
        moveN(1, Direction.Left) `sequence`
        goUntil(Direction.Left, t -> t != "s");
    let result = runMachine("_", 0, ms -> {println(ms.tape); Thread.sleep(Time.Duration.fromMilliSeconds(500))}, machine);
    println(result)
}
