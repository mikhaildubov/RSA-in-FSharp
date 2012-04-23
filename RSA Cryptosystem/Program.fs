namespace HSE.Dubov.RSA

/// The RSA Demo
module Program =
    
    let RSA_Demo bits msg = 

        let (publ, priv) = RSA.keys bits 3
        let encrypted = RSA.encrypt publ msg
        let decrypted = RSA.decrypt priv encrypted

        printfn "message = %A" msg;
        printfn "encrypted = %A" encrypted;
        printfn "decrypted = %A" decrypted;

    let Time f =
        let st = new System.Diagnostics.Stopwatch()
        st.Start()
        let returnValue = f();
        printf "Time: %i\n" st.ElapsedMilliseconds;
        st.Stop();
        returnValue


    Time (fun() -> RSA_Demo 16 1099I)