(* Jogging is © 2011 Victor Nicollet *)
val to_js    : JsCode.t -> View.Context.text View.t
val to_event : JsCode.t -> View.Context.text View.t
val to_json  : JsCode.t -> Json_type.t

val delay      : float -> JsCode.t -> JsCode.t
val form       : Id.t -> Json_type.t -> Json_type.t -> JsCode.t
val staticInit : JsCode.t
val defineBox  : string -> Id.t -> JsCode.t
val fillBox    : string -> View.Context.box View.t -> JsCode.t
val init       : string -> JsCode.t
val boxRefresh : float -> JsCode.t
val boxLoad    : string -> JsCode.t
val post       : string -> Json_type.t -> JsCode.t
val boxInvalidate : JsCode.t

type source

val source : name:string -> args:Json_type.t list -> source
val source_to_json : source -> Json_type.t

type render

val render : name:string -> args:Json_type.t list -> render
val render_to_json : render -> Json_type.t
