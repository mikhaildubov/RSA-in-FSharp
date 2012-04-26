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

        miller_rabin x s