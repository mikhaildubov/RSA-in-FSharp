namespace HSE.Dubov.RSA

/// Contains functions that generate random bigint's.
module Random =

    /// Generates a bigint in range [<c>0</c>, <c>max</c>).
    /// Requires a <c>System.Random object</c> to be passed as a parameter.
    let next_bigint (rand:System.Random) max =
        let rec next' (r:System.Random) m acc =
            let nxt = acc*2I + new bigint(r.Next(2))
            if nxt >= max then acc
            else next' r max nxt
        next' rand max 0I


    /// Generates a bigint of specified bit-length.
    /// Requires a <c>System.Random object</c> to be passed as a parameter.
    let next_bigint_bits (rand:System.Random) bits =
        let rec next' (r:System.Random) b acc =
            if b = 0 then acc
            else next' r (b-1) (acc*2I + new bigint(r.Next(2)))
        next' rand (bits-1) 1I

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
        let rec search_prime x =
            if ((Algorithms.is_prime x s) && (predicate x)) then x
            else search_prime (x + 2I)
        let random_bigint = next_bigint_bits rand bits
        let candidate = if (random_bigint % 2I = 0I) then (random_bigint + 1I) else random_bigint
        search_prime candidate