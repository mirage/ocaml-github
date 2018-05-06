(*
   Adapters used by atdgen to turn Github's representation of variants
   into an ATD-compatible representation.
*)
module Adapter = struct
  module Ref =
    Atdgen_runtime.Util.Json.Adapter.Type_and_value_fields.Make (struct
      let type_field_name = "ref_type"
      let value_field_name = "ref"
    end)

  module Changes =
    Atdgen_runtime.Util.Json.Adapter.Type_and_value_fields.Make (struct
      let type_field_name = "action"
      let value_field_name = "changes"
    end)

  module Payload =
    Atdgen_runtime.Util.Json.Adapter.Type_and_value_fields.Make (struct
      let type_field_name = "type"
      let value_field_name = "payload"
    end)

  module Config =
    Atdgen_runtime.Util.Json.Adapter.Type_and_value_fields.Make (struct
      let type_field_name = "name"
      let value_field_name = "config"
    end)
end

