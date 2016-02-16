open Kaputt.Abbreviations

let (|>) = Pervasives.(|>)
let tmppath_re = Str.regexp "/tmp/test[0-9a-fA-F]*/[^\"']*\\|/tmp/jstools[0-9a-fA-F]*/instrumented/[^\"']*"
let basename_re = Str.regexp "\\${basename}"

let generate_substituted pattern replacement input =
  let output = Filename.temp_file "tmp" "" in
  let lines = BatFile.lines_of input in
  let lines' = BatEnum.map (Str.global_replace pattern replacement) lines in
    BatFile.write_lines output lines';
    output

let copy_file input output = BatFile.write_lines output (BatFile.lines_of input)

let same_file expected got =
  (* Normalize the ``got'' file a bit. *)
  let got' = generate_substituted tmppath_re "..." got in
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

let test_instrument_for_browser =
  Test.make_assert_test
    ~title:"Instrumenting a JavaScript file for the browser, no basename"
    (fun () -> ())
    (fun () ->
       let base = JalangiInterface.instrument_for_browser
         (fun provided -> copy_file "testdata/test-instrument.js" provided; Lwt.return_unit)
       in let base = Lwt_main.run base in
       let expected = generate_substituted basename_re (Filename.basename base)
                        "testdata/test-instrument.browser.html" in
         same_file expected (base ^ ".html");
         same_file "testdata/test-instrument.instrumented.js" (base ^ ".js"))
    (fun () -> ())

let test_instrument_for_browser_basename =
  Test.make_assert_test
    ~title:"Instrumenting a JavaScript file for the browser, with basename"
    (fun () -> ())
    (fun () ->
       let base = JalangiInterface.instrument_for_browser ~basename:"base"
         ~providejs:(fun provided -> copy_file "testdata/test-instrument.js" provided;
                                     Lwt.return_unit)
       in let base = Lwt_main.run base in
       let expected = generate_substituted basename_re (Filename.basename base)
                        "testdata/test-instrument.browser.html" in
         Assert.equal_string ~msg:"base" "base" (Filename.basename base);
         same_file expected (base ^ ".html");
         same_file "testdata/test-instrument.instrumented.js" (base ^ ".js"))
    (fun () -> ())
    
let _ = Test.run_tests [ test_jalangi_instrument;
                         test_instrument_for_browser;
                         test_instrument_for_browser_basename ]
