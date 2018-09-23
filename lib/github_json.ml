(*
   Adapters used by atdgen to turn Github's representation of variants
   into an ATD-compatible representation.
*)
module Adapter = struct
  module Ref =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
      let type_field_name = "ref_type"
      let value_field_name = "ref"
      let known_tags = None
    end)

  module Payload =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
      let type_field_name = "type"
      let value_field_name = "payload"
      let known_tags = None
    end)

  module Issue_comment_event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
      let type_field_name = "action"
      let value_field_name = "changes"
      let known_tags =
        Some (["created"; "edited"; "deleted"], "Unknown")
    end)

  module Issues_event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
      let type_field_name = "action"
      let value_field_name = "changes"
      let known_tags =
        Some (
          [
            "assigned";
            "unassigned";
            "labeled";
            "unlabeled";
            "opened";
            "edited";
            "closed";
            "reopened";
          ],
          "Unknown"
        )
    end)

  module Pull_request_event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
      let type_field_name = "action"
      let value_field_name = "changes"
      let known_tags =
        Some (
          [
            "assigned";
            "unassigned";
            "labeled";
            "unlabeled";
            "opened";
            "edited";
            "closed";
            "reopened";
            "synchronize";
          ],
          "Unknown"
        )
    end)

  module Pull_request_review_comment_event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
      let type_field_name = "action"
      let value_field_name = "changes"
      let known_tags =
        Some (["created"; "edited"; "deleted"], "Unknown")
    end)

  module Event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
      let type_field_name = "type"
      let value_field_name = "payload"
      let known_tags =
        Some (
          [
            "CommitCommentEvent";
            "CreateEvent";
            "DeleteEvent";
            "DownloadEvent";
            "FollowEvent";
            "ForkEvent";
            "ForkApplyEvent";
            "GistEvent";
            "GollumEvent";
            "IssueCommentEvent";
            "IssuesEvent";
            "MemberEvent";
            "PublicEvent";
            "PullRequestEvent";
            "PullRequestReviewCommentEvent";
            "PushEvent";
            "ReleaseEvent";
            "RepositoryEvent";
            "StatusEvent";
            "WatchEvent";
          ],
          "Unknown"
        )
    end)

  module Hook =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
      let type_field_name = "name"
      let value_field_name = "config"
      let known_tags = Some (["web"], "Unknown")
    end)
end

