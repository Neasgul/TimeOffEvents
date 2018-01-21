namespace TimeOff



module App =
    open Newtonsoft.Json
    open Newtonsoft.Json.Serialization
    open Suave
    open Suave.Operators
    open Suave.Http
    open Suave.Successful
    open TimeOff

    [<EntryPoint>]
    let main argv =
        let commandWebPart = {
            Create = Db.createRequest
        }

        let app =
          choose [
                Router.rest "test" commandWebPart
                Router.rest "/test" commandWebPart
                RequestErrors.NOT_FOUND "Found no handlers"
          ]

        startWebServer defaultConfig app

        0