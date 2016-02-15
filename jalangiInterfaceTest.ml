open Kaputt.Abbreviations

let (|>) = Pervasives.(|>)
let same_file expected got =
  Assert.make_equal (=) Odiff.string_of_diffs
    [] (Odiff.files_diffs expected got)

let test_jalangi_instrument =
  Test.make_assert_test ~title:"Instrumenting a JavaScript file, simple"
    (fun () -> Lwt_main.run (Files.temp_dir_lwt "test" ""))
    (fun tmpdir ->
       JalangiInterface.jalangi2_instrument "xhr"
         [ "testdata/test-instrument.js"; "testdata/test-instrument.html" ]
         tmpdir
       |> Lwt_main.run |> ignore;
       same_file "testdata/test-instrument.instrumented.js"
         (tmpdir ^ "/test-instrument.js");
       same_file "testdata/test-instrument.instrumented.html"
         (tmpdir ^ "/test-instrument.html");
       tmpdir)
    (fun tmpdir -> FileUtil.rm ~force:FileUtil.Force ~recurse:true [tmpdir])

let _ = Test.run_tests [ test_jalangi_instrument ]
