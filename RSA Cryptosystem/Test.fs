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