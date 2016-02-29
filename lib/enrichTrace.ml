module Make(T: Streaming.Transformers) = struct
  module Step1 = LocalFacts.CollectArguments(T)
  module Step2 = LocalFacts.CollectClosures(T)
  module Step3 = CalculateNameBindings.Make(T)
  module Step4 = CalculateVersions.Make(T)
  module Step5 = CalculatePointsTo.Make(T)
  let collect initials trace =
    trace
      |> Step1.collect
      |> Step2.collect
      |> Step3.collect initials
      |> Step4.collect initials
      |> Step5.collect initials
end
