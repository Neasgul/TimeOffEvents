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
        let timeOff = Router.rest "timeOff" {
            Create = Db.createRequest
            GetById = Db.getUserRequests
        }

        let cancelTimeOff = Router.rest "cancelTimeOff" {
            Create = Db.cancelRequest
            GetById = Db.dummyGetById
        }

        let validateTimeOff = Router.rest "manager/validateTimeOff" {
            Create = Db.validateRequest
            GetById = Db.dummyGetById
        }

       
        let app = choose[timeOff;cancelTimeOff;validateTimeOff; RequestErrors.NOT_FOUND "Found no handlers"]
        startWebServer defaultConfig app

        0