module Make(T: Streaming.Transformers) = struct
  module Arguments = LocalFacts.CollectArguments(T)
  module Closures = LocalFacts.CollectClosures(T)
  module NameBindings = CalculateNameBindings.Make(T)
  module Versions = CalculateVersions.Make(T)
  module PointsTo = CalculatePointsTo.Make(T)
  module Prototypes = CalculatePrototypes.Make(T)
  module ActualBase = CalculateActualBase.Make(T)
  let collect initials trace =
    trace
      |> Arguments.collect
      |> Closures.collect
      |> NameBindings.collect initials
      |> Prototypes.collect initials
      |> ActualBase.collect initials
      |> Versions.collect initials
      |> PointsTo.collect initials
end
