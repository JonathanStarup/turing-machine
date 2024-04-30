use Functor.map

/// Theoretically, t should be a finite type.
enum Tape[t] {
    case Tape({
        left = List[t],
        middle = t,
        right = List[t],
        zero = t
    })
}

instance ToString[Tape[t]] with ToString[t] {
    pub def toString(x: Tape[t]): String = {
        use ToString.toString;
        let t = ex(x);
        def tapeString(xx: List[t]) = xx |> map(toString) |> String.intercalate("");
        let lstring = tapeString(t.left |> List.reverse);
        let padding = String.repeat(String.length(lstring), " ");
        "${padding}v\n${lstring}${t.middle |> Console.green}${tapeString(t.right)}"
    }
}

def ex(t: Tape[t]): {left = List[t], middle = t, right = List[t], zero = t} = match t {
    case Tape.Tape(x) => x
}

def popOrElse(l: List[t], zero: t): (t, List[t]) = match l {
    case Nil => (zero, Nil)
    case next :: tail => (next, tail)
}

def moveLeft(t0: Tape[t]): Tape[t] = {
    let t = ex(t0);
    let (middle, left) = popOrElse(t.left, t.zero);
    Tape.Tape({
        left = left,
        middle = middle,
        right = t.middle :: t.right,
        zero = t.zero
    })
}

def moveRight(t0: Tape[t]): Tape[t] = {
    let t = ex(t0);
    let (middle, right) = popOrElse(t.right, t.zero);
    Tape.Tape({
        left = t.middle :: t.left,
        middle = middle,
        right = right,
        zero = t.zero
    })
}

def setMiddle(middle: t, t: Tape[t]): Tape[t] = {
    t |> ex |> (x -> {middle = middle | x}) |> Tape.Tape
}

def tapeOf(initMargin: Int32, zero: t): Tape[t] = {
    let margin = List.repeat(initMargin, zero);
    Tape.Tape({
        left = margin,
        middle = zero,
        right = margin,
        zero = zero
    })
}

enum Direction {case Left, Stay, Right}

def moveDir(d: Direction, t: Tape[t]): Tape[t] = match d {
    case Direction.Left => moveLeft(t)
    case Direction.Stay => t
    case Direction.Right => moveRight(t)
}

/// Theoretically, t and state should be finite types.
type alias Machine[t, state] = {
    start = state,
    transition = (t, state) -> (t, Direction, state),
    end = state
}

type alias MachineRunState[t, state] = {
    currentState = state,
    tape = Tape[t]
}

type alias Monitor[t: Type, state: Type, ef: Eff] = MachineRunState[t, state] -> Unit \ ef

/// crash with monitor to timeout the computation
def runMachine(zero: t, initMargin: Int32, monitor: Monitor[t, state, ef], m: Machine[t, state]): Tape[t] \ ef with Eq[state]= {
    runMachineAux(m, monitor, tapeOf(initMargin, zero), m.start)
}

def runMachineAux(m: Machine[t, state], monitor: Monitor[t, state, ef], t: Tape[t], current: state): Tape[t] \ ef with Eq[state] = {
    let rec = runMachineAux(m, monitor);
    if (current == m.end)
        t
    else {
        monitor({currentState = current, tape = t});
        let (elm, dir, next) = m.transition(ex(t).middle, current);
        let nextTape = t |> setMiddle(elm) |> moveDir(dir);
        rec(nextTape, next)
    }
}

enum SimpleState with Eq {case Running, Done}
/// go direction until middle is true under the given predicate
def goUntil(d: Direction, pred: t -> Bool): Machine[t, SimpleState] = {
    use SimpleState.{Running, Done};
    def transition(t, state) = match state {
        case Done => (t, Direction.Stay, Done)
        case Running =>
            if (pred(t))
                (t, Direction.Stay, Done)
            else
                (t, d, Running)
    };
    {transition = transition, start = Running, end = Done}
}

enum NumState with Eq {case Running(Int32), Done}
def mapN(n: Int32, d: Direction, f: t -> t): Machine[t, NumState] = {
    use NumState.{Running, Done};
    def transition(t, state) = match state {
        case Done => (t, Direction.Stay, Done)
        case Running(i) if i <= 0 => (t, Direction.Stay, Done)
        case Running(1) => (f(t), d, Done)
        case Running(i) => (f(t), d, Running(i-1))
    };
    {transition = transition, start = Running(n), end = Done}
}

def repeatN(n: Int32, d: Direction, elm: t): Machine[t, NumState] = {
    mapN(n, d, _ -> elm)
}

def moveN(n: Int32, d: Direction): Machine[t, NumState] = {
    mapN(n, d, t -> t)
}

enum Either[a, b] {
    case Left(a)
    case Right(b)
}

instance Eq[Either[a, b]] with Eq[a], Eq[b] {
    pub def eq(x: Either[a, b], y: Either[a, b]): Bool = match (x, y) {
        case (Either.Left(xx), Either.Left(yy)) => xx == yy
        case (Either.Right(xx), Either.Right(yy)) => xx == yy
        case _ => false
    }
}

def mapThird(f: c -> d \ ef, p: (a, b, c)): (a, b, d) \ ef = match p { case (a, b, c) => (a, b, f(c)) }

def sequence(m1: Machine[t, state1], m2: Machine[t, state2]): Machine[t, Either[state1, state2]] with Eq[state1] = {
    use Either.{Left, Right};
    def transition(t, state) = match state {
        case Left(state1) if state1 == m1.end =>
            m2.transition(t, m2.start) |> mapThird(Right)
        case Left(state1) =>
            m1.transition(t, state1) |> mapThird(Left)
        case Right(state2) =>
            m2.transition(t, state2) |> mapThird(Right)
    };
    {transition = transition, start = Left(m1.start), end = Right(m2.end)}
}
