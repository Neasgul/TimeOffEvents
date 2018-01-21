namespace TimeOff

open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Suave
open Suave.Operators
open Suave.Http
open Suave.Successful
open TimeOff

module People =
    open Suave.RequestErrors
    open Suave.Filters

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm = System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>
    
    let rest path =
        choose
            [ GET >=> choose
                [ path "/hello" >=> OK "Hello GaazeET"
                  path "/goodbye" >=> OK "Good bye GET" ]
              POST >=> choose
                [ path "/hello" >=> OK "Hello POST"
                  path "/goodbye" >=> OK "Good bye POST" ] ]