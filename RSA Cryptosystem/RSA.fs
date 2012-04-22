namespace HSE.Dubov.RSA

/// The implementation of basic RSA algorithms.
module RSA =
    
    /// Generates the keys. The public exponent is to be defined by the user (the <c>e</c> parameter).
    /// Returns the tuple (public, private), where the public key is a tuple (n, e) and the private key - (n, d)
    let keys bits (e:int) =
        let rand = new System.Random()
        let p = Random.next_prime_predicate rand (bits/2) 30 (fun x -> (Algorithms.gcd (x-1I) (new bigint(e))) = 1I)
        let q = Random.next_prime_predicate rand (bits/2) 30 (fun x -> (Algorithms.gcd (x-1I) (new bigint(e))) = 1I && x <> p)
        let n = p*q
        let phi_n = (p-1I)*(q-1I)
        let d = Algorithms.mod_inv (new bigint(e)) phi_n

        printfn "p = %A" p;
        printfn "q = %A" q;
        printfn "n = %A" n;
        printfn "phi(n) = %A" phi_n;
        printfn "public (n, e) = (%A, %A)" n e;
        printfn "private (n, d) = (%A, %A)" n d;

        ((n, (new bigint(e))), (n, d))


    /// Encrypts the message using the public key
    let encrypt msg (n, e) =
        Algorithms.mod_exp msg e n

    /// Decrypts the message using the private key
    let decrypt enc (n, d) =
        Algorithms.mod_exp enc d n