open System

type BiSquareRootResult = 
    | NoRoots
    | TwoRoots of double*double
    | FourRoots of double*double*double*double

///Функция вычисления корней уравнения
let CalculateRoots(a:double, b:double, c:double):BiSquareRootResult = 
    let D = b*b - 4.0*a*c
    if D < 0.0 then NoRoots
    else if D = 0.0 then 
        let expression = -b/(2.0*a)
        if expression < 0.0 then NoRoots
        else TwoRoots (Math.Sqrt(expression), -Math.Sqrt(expression))
    else 
        let sqrtD = Math.Sqrt(D)
        let expression1 = (-b + sqrtD)/(2.0*a)
        let expression2 = (-b - sqrtD)/(2.0*a)
        if expression1 < 0.0 then
            if expression2 < 0.0 then NoRoots
            else TwoRoots (Math.Sqrt(expression2), -Math.Sqrt(expression2))
        else
            if expression2 < 0.0 then TwoRoots (Math.Sqrt(expression1), -Math.Sqrt(expression1))
            else FourRoots (Math.Sqrt(expression1), -Math.Sqrt(expression1), Math.Sqrt(expression2), -Math.Sqrt(expression2))

///Функция вывода корней уравнения
let PrintRoots(a:double, b:double, c:double):unit = 
    printf "Коэффициенты: a=%A, b=%A, c=%A. " a b c
    let roots = CalculateRoots(a, b, c)
    let textResult = 
        match roots with
        | NoRoots -> "Корней нет\n"
        | TwoRoots(root1, root2) -> "Два корня: " + root1.ToString() + " и " + root2.ToString() + "\n"
        | FourRoots(root1, root2, root3, root4) -> "Четыре корня: " + root1.ToString() + ", " + root2.ToString() + ", " + root3.ToString() + " и " + root4.ToString() + "\n"
    printf "%s" textResult
        


[<EntryPoint>]
let main argv = 
    //Тестовые данные
    //4 корня
    let a1 = 4.0
    let b1 = -5.0
    let c1 = 1.0
    PrintRoots(a1, b1, c1)

    //2 корня
    let a2 = 1.0
    let b2 = -2.0
    let c2 = -8.0
    PrintRoots(a2, b2, c2)

    //нет корней
    let a3 = 1.0
    let b3 = 11.0
    let c3 = 10.0
    PrintRoots(a3, b3, c3)

    Console.ReadLine() |> ignore
    0 // возвращение целочисленного кода выхода
