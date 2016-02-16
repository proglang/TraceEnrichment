open Kaputt.Abbreviations

let (|>) = Pervasives.(|>)
let tmppath_re = Str.regexp "/tmp/test[0-9a-fA-F]*/"

let same_file expected got =
  (* Normalize the ``got'' file a bit. *)
  let got' = Filename.temp_file "result" "" in
  let lines = BatFile.lines_of got in
  let lines' = BatEnum.map (Str.global_replace tmppath_re "out/") lines in
    BatFile.write_lines got' lines';
  Assert.equal_int ~msg:"Diffing files, return value" 0
    (Sys.command ("diff -u -w -B " ^ expected ^ " " ^ got'))

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
