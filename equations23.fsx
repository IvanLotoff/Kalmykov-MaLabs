let e = 0.000001

let rec d f a b =
    let m = (b+a) * 0.5
    match f(m) with
        | value when value = 0. || (b - a) < e -> m
        | value when value * (f(b) - f(a)) < 0. -> d f m b
        | _ -> d f a m

let rec iter phi x0 =
    if System.Double.IsInfinity(x0)||System.Double.IsNaN(x0) then x0
    else 
    let x1 = phi(x0)
    if abs(x1 - x0) > e then iter phi x1
    else x1

let newthon func func' x0 =
    iter (fun x -> x - func(x)/func'(x)) x0

let func1 x = 3.*x-4.*System.Math.Log (x)-5.
let func2 x = System.Math.Cos (2./x)-2.*System.Math.Sin(1./x)+1./x
let func3 x = sqrt(1.-0.4*x**2.)-System.Math.Asin(x)

let func1' x = 3.-4./x
let func2' x = (-1.+2.*System.Math.Cos(1./x)+2.*System.Math.Sin(2./x))/x**2.
let func3' x = -1./sqrt(1.-x**2.)-(0.4*x)/sqrt(1.-0.4*x**2.)

let phi = fun f c x -> x + c * f(x)
let phi1 = phi func3 1.
let phi2 = phi func2 -0.05
let phi3 = phi func3 0.1 

let main = 
    printfn "%10.5f  %10.5f  %10.5f  %10.5f" 1.1474 (d func3 1. 1.5) (iter phi1 0.) (newthon func3 func1' 1.)
    printfn "%10.5f  %10.5f  %10.5f  %10.5f" 2.0692 (d func2 1. 3.) (iter phi2 2.) (newthon func2 func2' 1.)
    printfn "%10.5f  %10.5f  %10.5f  %10.5f" 0.5768 (d func3 0. 1.) (iter phi3 0.) (newthon func3 func3' 0.5)