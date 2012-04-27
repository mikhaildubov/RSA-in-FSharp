namespace HSE.Dubov.RSA

/// Contains implementations of the basic numerical algorithms used in RSA.
module Algorithms =
    
    /// Binary modular exponentiation.
    let rec mod_exp a b n = 
        if b = 0I then 1I
        else let z = mod_exp a (b/2I) n
             if (b % 2I = 0I) then ((z*z) % n)
                              else ((a*z*z) % n)


    /// Modular inverse. Uses extended Euclid algorithm.
    let mod_inv a n =

        let rec ext_euclid a b =
            if (b = 0I) then (a, 1I, 0I) 
            else let (d', x', y') = ext_euclid b (a % b)
                 let (d, x, y) = (d', y', x' - (a/b)*y')
                 (d, x, y)

        let (d, x, y) = ext_euclid a n
        if (x < 0I) then (x+n) else x


    /// Greatest common divistor. Uses the Euclid algorithm.
    let rec gcd a b =
        if b = 0I then a
        else gcd b (a % b)

    
    /// Array of small prime numbers. Used in primality test for optimization.
    let small_primes = [|2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I; 29I; 31I; 37I; 41I; 43I; 47I; 53I; 59I; 61I; 67I; 71I; 73I;
                         79I; 83I; 89I; 97I; 101I; 103I; 107I; 109I; 113I; 127I; 131I; 137I; 139I; 149I; 151I; 157I; 163I; 
                         167I; 173I; 179I; 181I; 191I; 193I; 197I; 199I; 211I; 223I; 227I; 229I; 233I; 239I; 241I; 251I;
                         257I; 263I; 269I; 271I; 277I; 281I; 283I; 293I; 307I; 311I; 313I; 317I; 331I; 337I; 347I; 349I;
                         353I; 359I; 367I; 373I; 379I; 383I; 389I; 397I; 401I; 409I; 419I; 421I; 431I; 433I; 439I; 443I;
                         449I; 457I; 461I; 463I; 467I; 479I; 487I; 491I; 499I; 503I; 509I; 521I; 523I; 541I; 547I; 557I;
                         563I; 569I; 571I; 577I; 587I; 593I; 599I; 601I; 607I; 613I; 617I; 619I; 631I; 641I; 643I; 647I;
                         653I; 659I; 661I; 673I; 677I; 683I; 691I; 701I; 709I; 719I; 727I; 733I; 739I; 743I; 751I; 757I;
                         761I; 769I; 773I; 787I; 797I; 809I; 811I; 821I; 823I; 827I; 829I; 839I; 853I; 857I; 859I; 863I;
                         877I; 881I; 883I; 887I; 907I; 911I; 919I; 929I; 937I; 941I; 947I; 953I; 967I; 971I; 977I; 983I; 991I; 997I|]


    /// Miller-Rabin probabilistic primality test.
    /// P(mistake) = 2^(<c>-s</c>), where <c>s</c> is the number of iterations.
    let is_prime x s =

        let get_tu n =
            let n' = n - 1I
            let rec calc_tu t u = 
                if u % 2I = 1I then (t, u)
                else calc_tu (t+1) (u/2I)
            calc_tu 0 n'

        let witness a n =
            let (t, u) = get_tu n
            let mutable prev = mod_exp a u n
            let mutable next = (prev*prev) % n
            let mutable nontrivial_roots = false
            for i = 1 to t do
                nontrivial_roots <- nontrivial_roots || (next = 1I && prev <> 1I && prev <> (n-1I));
                prev <- next; next <- (prev*prev) % n
            if (prev <> 1I || nontrivial_roots) then true
            else false

        let miller_rabin n s =
            let rand = Random.bigint_generator (n-2I)
            let rec test c = 
                if c = 0 then true
                else
                    let a = rand() + 2I
                    //printf "a = %A " a;
                    if (witness a n) then false
                    else test (c-1)
            test s

        let divisible_by_small_prime n =
            let rec check i = 
                if i = 168
                    then false
                    else if n > small_primes.[i]
                            then if (n % small_primes.[i] = 0I)
                                    then true
                                    else check (i+1)
                            else false
            check 0

        if (divisible_by_small_prime x) // Optimization using a table of small primes
            then false
            else if (x = 1I || Array.exists (fun elem -> elem = x) small_primes) // => very small keys (p < 1000)
                    then true
                    else (miller_rabin x s) // Miller-Rabin test call