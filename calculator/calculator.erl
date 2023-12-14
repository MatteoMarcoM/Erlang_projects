-module(calculator).
-import(c, [flush/0]).
-export([
    calculate/0,
    init/0,
    demo/0
]).

% server calculator (loop)
calculate() -> 
    receive
        % con calculator:calculate() usa l'ultima versione del codice senza
        % fare restart del server (hot swap)
        % somma
        {add, X, Y, Pid} -> Pid ! {somma, X, Y, X + Y}, calculator:calculate();
        % senza calculator: per applicare le modifiche al codice bisogna 
        % rieseguire il server
        {sum, X, Y, Pid} -> Pid ! {somma, X, Y, X + Y}, calculate();
        % sottrazione
        {sub, X, Y, Pid} -> Pid ! {differenza, X, Y, X - Y}, calculator:calculate();
        % moltiplicazione
        {mult, X, Y, Pid} -> Pid ! {moltiplicazione, X, Y, X * Y}, calculator:calculate();
        % divisione intera
        {div_int, X, Y, Pid} -> Pid ! {divisone_intera, X, Y, X div Y}, calculator:calculate();
        % terminare il server (no loop)
        {stop} -> ok
    end
.

% initialize calculator
init() -> spawn(calculator, calculate, []).

% demo 10 + 20, 1 + 2
demo() -> 
    % svuota la coda della shell
    % da importare !
    c:flush(),
    PidCalc = init(),
    PidCalc!{add, 10, 20, self()},
    PidCalc!{sum, 3, 7, self()},
    PidCalc!{sub, 4, 9, self()},
    PidCalc!{mult, 8, 2, self()},
    PidCalc!{div_int, 7, 2, self()},
    PidCalc!{stop} 
.
