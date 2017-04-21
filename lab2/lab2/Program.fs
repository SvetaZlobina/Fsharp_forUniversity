open System

///Некаррированная функция, возвращающая кортеж
let MultUncurry(a, b, c) = (a*b, a*b*c)

///Каррированная функция, возвращающая кортеж
let MultCurry(a)(b)(c) = (a*b, a*b*c)

///Рекурсивная функция подсчёта суммы целых чисел в интервале
let rec SumInterval(min:int, max:int):int = 
    if (min+1 = max) then 0 //интервал пуст
    else min+1+SumInterval(min+1, max)

///Рекурсивная функция подсчёта суммы целых чисел в интервале с хвостовой рекурсией
let rec SumIntervalTail(min:int, max:int, sum:int):int = 
    if (min+1 = max) then sum //интервал пуст
    else SumIntervalTail(min+1, max, sum+min+1)

///Обёртка для сокрытия хвостовой рекурсии
let rec SumInterval2(min, max) = SumIntervalTail(min, max, 0)

///Конечный автомат из трёх состояний в виде взаимно-рекурсивных функций
let rec State1(x:int) = 
    printfn "%i - (+1) %i" x (x+1)
    let xNext = x + 1
    if xNext > 5 then State2(xNext)
    else State1(xNext)
and State2(x:int) = 
    printfn "%i - (*2) %i" x (x*2)
    let xNext = x * 2
    if xNext > 50 then State3(xNext)
    else State2(xNext)
and State3(x:int) =
    printfn "%i - (+10) %i" x (x+10)
    let xNext = x + 10
    if xNext <= 90 then State3(xNext)

///Функция-сумматор с параметрами в виде кортежа
let SumFunc1(a:int, b:int, c:int, func:int*int*int->int) = func(a, b, c)

///Функция-сумматор с параметрами в каррированном виде
let SumFunc2(a:int, b:int, c:int, func:int->int->int->int) = func a b c


[<EntryPoint>]
let main argv = 
    //Пункт 1
    let (multUncurry2, multUncurry3) = MultUncurry(1, 2, 3)
    let (multCurry2, multCurry3) = MultCurry 1 2 3
    //let multCurry2 = MultCurry 1 2
    //let multCurry3 = MultCurry 3
    printfn "Кортеж, полученный из некаррированной функции: %i %i" multUncurry2 multUncurry3 
    printfn "Кортеж, полученный из каррированной функции: %i %i\n" multCurry2 multCurry3

    //Пункт 2
    let sumInterval = SumInterval(10, 15)
    printfn "Сумма целых чисел в интервале от %i до %i равна %i" 10 15 sumInterval

    //Пункт 3
    let sumInterval2 = SumInterval2(10, 15)
    printfn "(Хвостовая рекурсия)Сумма целых чисел в интервале от %i до %i равна %i\n" 10 15 sumInterval2

    //Пункт 4
    printfn "Конечный автомат из трёх состояний:"
    State1(2)
    printfn ""

    //Пункт 5
    let sumFuncCall1 = SumFunc1(1, 2, 3, fun(a, b, c)->a+b+c)
    printfn "Функция-сумматор с параметрами в виде кортежа: %i" sumFuncCall1
    let sumFuncCall2 = SumFunc2(1, 2, 3, fun a b c->a+b+c)
    printfn "Функция-сумматор с параметрами в каррированном виде: %i" sumFuncCall2

    Console.ReadLine() |> ignore
    //printfn "%A" argv
    0 // возвращение целочисленного кода выхода
