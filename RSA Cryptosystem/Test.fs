open HSE.Dubov.RSA

/// Tests the main algorithms
module Test = 
    
    Algorithms.mod_exp 7I 560I 561I;; // 7^560 mod 561 = 1

    Algorithms.mod_inv 5I 11I;; // 5^(-1) mod 11 = 9

    Algorithms.gcd 5I 11I;; // gcd(5, 11) = 1

    Algorithms.is_prime 561I 30;;  // Carmichael number
    Algorithms.is_prime 1105I 30;; // Carmichael number
    Algorithms.is_prime 1729I 30;; // Carmichael number
    Algorithms.is_prime 997I 30;;  // Prime number
    Algorithms.is_prime 5963I 30;; // Composite number
    
    Random.next_bigint_bits (new System.Random()) 3;;

    let rand = Random.bigint_generator 100I;;
    let a = Array.create 10 0;;
    for i=1 to 10000 do
        let next = rand()
        a.[(int)(next)/10] <- a.[(int)(next)/10]+1;;
    for i=0 to 9 do
        printf "%A " a.[i];;