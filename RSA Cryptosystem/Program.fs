namespace HSE.Dubov.RSA

/// The RSA Demo
module Program =
    
    let RSA_Demo bits msg = 

        let (publ, priv) = RSA.keys bits 3
        let encrypted = RSA.encrypt msg publ
        let decrypted = RSA.encrypt encrypted priv

        printfn "message = %A" msg;
        printfn "encrypted = %A" encrypted;
        printfn "decrypted = %A" decrypted;

    RSA_Demo 16 1099I