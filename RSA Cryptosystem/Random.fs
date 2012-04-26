namespace HSE.Dubov.RSA

/// Contains functions that generate random bigint's.
module Random =

    /// Generates a bigint of specified bit-length.
    /// Requires a <c>System.Random object</c> to be passed as a parameter.
    let next_bigint_bits (rand:System.Random) bits =
        let rec next' (r:System.Random) b acc =
            if b <= 0 then acc
            else next' r (b-1) (acc*2I + new bigint(r.Next(2)))
        next' rand (bits-1) 1I // To preserve the bit length


    /// Cell type used in bigint_generator.
    type cell = {
                    table : bigint array
                    m : bigint
                    mutable j : int
                    mutable k : int
                }

    /// An additive random bigint generator.
    /// Generates bigint's in range [<c>0</c>, <c>n</c>).
    /// Uses algorithm 3.2.2A from TAOCP.
    let bigint_generator n =
        let x = {
                    table = Array.create 55 0I
                    m = n
                    j = 23
                    k = 54
                }
        let rand = (new System.Random())
        let bits_max = (int)(System.Numerics.BigInteger.Log (n, 2.))
        for i=0 to 54 do x.table.[i] <- (next_bigint_bits rand (rand.Next(bits_max)+1));
        fun () -> (x.table.[x.k] <- (x.table.[x.k] + x.table.[x.j]) % x.m;
                   let res = x.table.[x.k];
                   x.j <- if x.j = 0 then 54 else (x.j - 1);
                   x.k <- if x.k = 0 then 54 else (x.k - 1);
                   res)


    /// Generates a prime bigint of specified bit-length.
    /// Requires a <c>System.Random object</c> to be passed as a parameter.
    /// The <c>s</c> parameter denotes the number of iterations in the primality test.
    let next_prime rand bits s =
        let rec search_prime x =
            if (Algorithms.is_prime x s) then x
            else search_prime (x + 2I)
        let random_bigint = next_bigint_bits rand bits
        let candidate = if (random_bigint % 2I = 0I) then (random_bigint + 1I) else random_bigint
        search_prime candidate


    /// Generates a prime bigint of specified bit-length that satisfies the given predicate.
    /// Requires a <c>System.Random object</c> to be passed as a parameter.
    /// The <c>s</c> parameter denotes the number of iterations in the primality test.
    let next_prime_predicate rand bits s predicate =
        let rec search_prime x attempts =
            if ((Algorithms.is_prime x s) && (predicate x)) then
                printfn "found a prime after %A attempts" attempts;
                x
            else search_prime (x + 2I) (attempts+1)
        let random_bigint = next_bigint_bits rand bits
        let candidate = if (random_bigint % 2I = 0I) then (random_bigint + 1I) else random_bigint
        search_prime candidate 1