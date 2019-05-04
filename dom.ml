[@@@comment "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
module Event =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x2 -> x2
    and (t_to_js : t -> Ojs.t) = fun x1 -> x1
    let (t_of_js : Ojs.t -> t) = t_of_js
    let (t_to_js : t -> Ojs.t) = t_to_js
    let (client_x : t -> int) =
      fun x5 -> Ojs.int_of_js (Ojs.get (t_to_js x5) "clientX")
    let (client_y : t -> int) =
      fun x6 -> Ojs.int_of_js (Ojs.get (t_to_js x6) "clientY")
    let (page_x : t -> float) =
      fun x7 -> Ojs.float_of_js (Ojs.get (t_to_js x7) "pageX")
    let (page_y : t -> float) =
      fun x8 -> Ojs.float_of_js (Ojs.get (t_to_js x8) "pageY")
    let (screen_x : t -> int) =
      fun x9 -> Ojs.int_of_js (Ojs.get (t_to_js x9) "screenX")
    let (screen_y : t -> int) =
      fun x10 -> Ojs.int_of_js (Ojs.get (t_to_js x10) "screenY")
    let (buttons : t -> int) =
      fun x11 -> Ojs.int_of_js (Ojs.get (t_to_js x11) "buttons")
    let (alt_key : t -> bool) =
      fun x12 -> Ojs.bool_of_js (Ojs.get (t_to_js x12) "altKey")
    let (ctrl_key : t -> bool) =
      fun x13 -> Ojs.bool_of_js (Ojs.get (t_to_js x13) "ctrlKey")
    let (shift_key : t -> bool) =
      fun x14 -> Ojs.bool_of_js (Ojs.get (t_to_js x14) "shiftKey")
    let (which : t -> int) =
      fun x15 -> Ojs.int_of_js (Ojs.get (t_to_js x15) "which")
    let (code : t -> string) =
      fun x16 -> Ojs.string_of_js (Ojs.get (t_to_js x16) "code")
    let (key : t -> string) =
      fun x17 -> Ojs.string_of_js (Ojs.get (t_to_js x17) "key")
  end
module Element =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x19 -> x19
    and (t_to_js : t -> Ojs.t) = fun x18 -> x18
    let (t_of_js : Ojs.t -> t) = t_of_js
    let (t_to_js : t -> Ojs.t) = t_to_js
    let (append_child : t -> t -> unit) =
      fun x23 ->
        fun x22 ->
          ignore (Ojs.call (t_to_js x23) "appendChild" [|(t_to_js x22)|])
    let (add_event_listener :
      t -> string -> (Event.t -> unit) -> bool -> unit) =
      fun x28 ->
        fun x24 ->
          fun x25 ->
            fun x27 ->
              ignore
                (Ojs.call (t_to_js x28) "addEventListener"
                   [|(Ojs.string_to_js x24);(Ojs.fun_to_js 1
                                               (fun x26 ->
                                                  x25 (Event.t_of_js x26)));(
                     Ojs.bool_to_js x27)|])
    let (get_elements_by_tag_name : t -> string -> t array) =
      fun x30 ->
        fun x29 ->
          Ojs.array_of_js t_of_js
            (Ojs.call (t_to_js x30) "getElementsByTagName"
               [|(Ojs.string_to_js x29)|])
    let (has_attribute : t -> string -> bool) =
      fun x33 ->
        fun x32 ->
          Ojs.bool_of_js
            (Ojs.call (t_to_js x33) "hasAttribute" [|(Ojs.string_to_js x32)|])
    let (get_attribute : t -> string -> string) =
      fun x35 ->
        fun x34 ->
          Ojs.string_of_js
            (Ojs.call (t_to_js x35) "getAttribute" [|(Ojs.string_to_js x34)|])
    let (set_attribute : t -> string -> string -> unit) =
      fun x38 ->
        fun x36 ->
          fun x37 ->
            ignore
              (Ojs.call (t_to_js x38) "setAttribute"
                 [|(Ojs.string_to_js x36);(Ojs.string_to_js x37)|])
    let (set_onclick : t -> (unit -> unit) -> unit) =
      fun x39 ->
        fun x40 ->
          Ojs.set (t_to_js x39) "onclick" (Ojs.fun_to_js 1 (fun _ -> x40 ()))
    let (value : t -> string) =
      fun x41 -> Ojs.string_of_js (Ojs.get (t_to_js x41) "value")
    let (selected_index : t -> int) =
      fun x42 -> Ojs.int_of_js (Ojs.get (t_to_js x42) "selectedIndex")
    let (checked : t -> bool) =
      fun x43 -> Ojs.bool_of_js (Ojs.get (t_to_js x43) "checked")
    let (set_checked : t -> bool -> unit) =
      fun x44 ->
        fun x45 -> Ojs.set (t_to_js x44) "checked" (Ojs.bool_to_js x45)
    let (set_text_content : t -> string -> unit) =
      fun x46 ->
        fun x47 -> Ojs.set (t_to_js x46) "textContent" (Ojs.string_to_js x47)
    let (set_class_name : t -> string -> unit) =
      fun x48 ->
        fun x49 -> Ojs.set (t_to_js x48) "className" (Ojs.string_to_js x49)
    let (class_name : t -> string) =
      fun x50 -> Ojs.string_of_js (Ojs.get (t_to_js x50) "className")
    let (width : t -> int) =
      fun x51 -> Ojs.int_of_js (Ojs.get (t_to_js x51) "width")
    let (height : t -> int) =
      fun x52 -> Ojs.int_of_js (Ojs.get (t_to_js x52) "height")
  end
module Document =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x54 -> x54
    and (t_to_js : t -> Ojs.t) = fun x53 -> x53
    let (t_of_js : Ojs.t -> t) = t_of_js
    let (t_to_js : t -> Ojs.t) = t_to_js
    let (create_element : t -> string -> Element.t) =
      fun x58 ->
        fun x57 ->
          Element.t_of_js
            (Ojs.call (t_to_js x58) "createElement"
               [|(Ojs.string_to_js x57)|])
    let (create_text_node : t -> string -> Element.t) =
      fun x60 ->
        fun x59 ->
          Element.t_of_js
            (Ojs.call (t_to_js x60) "createTextNode"
               [|(Ojs.string_to_js x59)|])
    let (get_element_by_id : t -> string -> Element.t) =
      fun x62 ->
        fun x61 ->
          Element.t_of_js
            (Ojs.call (t_to_js x62) "getElementById"
               [|(Ojs.string_to_js x61)|])
    let (get_elements_by_class_name : t -> string -> Element.t array) =
      fun x64 ->
        fun x63 ->
          Ojs.array_of_js Element.t_of_js
            (Ojs.call (t_to_js x64) "getElementsByClassName"
               [|(Ojs.string_to_js x63)|])
    let (body : t -> Element.t) =
      fun x66 -> Element.t_of_js (Ojs.get (t_to_js x66) "body")
    let (set_title : t -> string -> unit) =
      fun x67 ->
        fun x68 -> Ojs.set (t_to_js x67) "title" (Ojs.string_to_js x68)
  end
module Window =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x70 -> x70
    and (t_to_js : t -> Ojs.t) = fun x69 -> x69
    let (t_of_js : Ojs.t -> t) = t_of_js
    let (t_to_js : t -> Ojs.t) = t_to_js
    let (add_event_listener :
      t -> string -> (Event.t -> unit) -> bool -> unit) =
      fun x77 ->
        fun x73 ->
          fun x74 ->
            fun x76 ->
              ignore
                (Ojs.call (t_to_js x77) "addEventListener"
                   [|(Ojs.string_to_js x73);(Ojs.fun_to_js 1
                                               (fun x75 ->
                                                  x74 (Event.t_of_js x75)));(
                     Ojs.bool_to_js x76)|])
  end
module Canvas =
  struct
    module RenderingContext2D =
      struct
        type t = Ojs.t
        let rec (t_of_js : Ojs.t -> t) = fun x79 -> x79
        and (t_to_js : t -> Ojs.t) = fun x78 -> x78
        let (line_width : t -> float -> unit) =
          fun x81 ->
            fun x80 ->
              ignore
                (Ojs.call (t_to_js x81) "lineWidth" [|(Ojs.float_to_js x80)|])
        let (set_fill_style : t -> string -> unit) =
          fun x82 ->
            fun x83 ->
              Ojs.set (t_to_js x82) "fillStyle" (Ojs.string_to_js x83)
        let (set_stroke_style : t -> string -> unit) =
          fun x84 ->
            fun x85 ->
              Ojs.set (t_to_js x84) "strokeStyle" (Ojs.string_to_js x85)
        let (fill_rect : t -> int -> int -> int -> int -> unit) =
          fun x90 ->
            fun x86 ->
              fun x87 ->
                fun x88 ->
                  fun x89 ->
                    ignore
                      (Ojs.call (t_to_js x90) "fillRect"
                         [|(Ojs.int_to_js x86);(Ojs.int_to_js x87);(Ojs.int_to_js
                                                                    x88);(
                           Ojs.int_to_js x89)|])
        let (stroke_rect : t -> int -> int -> int -> int -> unit) =
          fun x95 ->
            fun x91 ->
              fun x92 ->
                fun x93 ->
                  fun x94 ->
                    ignore
                      (Ojs.call (t_to_js x95) "strokeRect"
                         [|(Ojs.int_to_js x91);(Ojs.int_to_js x92);(Ojs.int_to_js
                                                                    x93);(
                           Ojs.int_to_js x94)|])
        let (line_to : t -> int -> int -> unit) =
          fun x98 ->
            fun x96 ->
              fun x97 ->
                ignore
                  (Ojs.call (t_to_js x98) "lineTo"
                     [|(Ojs.int_to_js x96);(Ojs.int_to_js x97)|])
        let (move_to : t -> int -> int -> unit) =
          fun x101 ->
            fun x99 ->
              fun x100 ->
                ignore
                  (Ojs.call (t_to_js x101) "moveTo"
                     [|(Ojs.int_to_js x99);(Ojs.int_to_js x100)|])
        let (stroke : t -> unit) =
          fun x102 -> ignore (Ojs.call (t_to_js x102) "stroke" [||])
        let (arc : t -> int -> int -> int -> int -> float -> unit) =
          fun x108 ->
            fun x103 ->
              fun x104 ->
                fun x105 ->
                  fun x106 ->
                    fun x107 ->
                      ignore
                        (Ojs.call (t_to_js x108) "arc"
                           [|(Ojs.int_to_js x103);(Ojs.int_to_js x104);(
                             Ojs.int_to_js x105);(Ojs.int_to_js x106);(
                             Ojs.float_to_js x107)|])
        let (begin_path : t -> unit) =
          fun x109 -> ignore (Ojs.call (t_to_js x109) "beginPath" [||])
      end
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x111 -> x111
    and (t_to_js : t -> Ojs.t) = fun x110 -> x110
    let (of_element : Element.t -> t) =
      fun x112 -> t_of_js (Element.t_to_js x112)
    let (get_context : t -> string -> Ojs.t) =
      fun x114 ->
        fun x113 ->
          Ojs.call (t_to_js x114) "getContext" [|(Ojs.string_to_js x113)|]
    let get_context_2d x = get_context x "2d"
  end
let (window : Window.t) = Window.t_of_js (Ojs.get Ojs.global "window")
let (document : Document.t) =
  Document.t_of_js (Ojs.get Ojs.global "document")