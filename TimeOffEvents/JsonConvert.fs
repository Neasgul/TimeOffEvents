namespace TimeOff

open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Suave
open Suave.Operators
open Suave.Http
open Suave.Successful

[<AutoOpen>]
module JsonConvert =
    let JSON v =
        let jsonSerializerSettings = new JsonSerializerSettings()
        jsonSerializerSettings.ContractResolver <- new CamelCasePropertyNamesContractResolver()

        JsonConvert.SerializeObject(v, jsonSerializerSettings)
        |> OK
        >=> Writers.setMimeType "application/json; charset=utf-8"

    let fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a
