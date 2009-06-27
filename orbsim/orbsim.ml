(* orbsim: simulates the orbit
 *)

(* TODO:
   + pan (aber keine floete)
   + trace
   + ruler
   + fadenkreuz wenn erde zu klein
*)

open GMain



let earth_r = 6357000.0
let initial_zoom = 7.0
let initial_speed = 10
let pi = atan 1. *. 4.0;;
let two_pi = pi *. 2.0;;

let rgb_red =     1.0, 0.0, 0.0
let rgb_green =   0.0, 1.0, 0.0
let rgb_blue =    0.0, 0.0, 1.0
let rgb_yellow =  1.0, 1.0, 0.0
let rgb_cyan =    0.0, 1.0, 1.0
let rgb_magenta = 1.0, 0.0, 1.0

let our_x = ref 0.0
let our_y = ref 0.0
let our_target_orbit = ref 0.0

let our_history : (float * float) list ref = ref []

(* 0,0 is always in middle of screen (coords of earth) *)
type space_screen = {
  mutable zoom :float; (* .. radius int space that is displayed around earth *)
  mutable speed :int; (* replay speed *)
  mutable screen_width :int;
  mutable screen_height :int;
  mutable screen_minlen :float;
  mutable screen_x_pre :float;
  mutable screen_y_pre :float;
  mutable spaceview_x1 :float;
  mutable spaceview_y1 :float;
  mutable spaceview_width :float;
  mutable spaceview_height :float;
  mutable spaceview_minlen :float; (* min (witdh, height) *)
}

let spasc_dump spasc =
  Printf.fprintf stderr "SPASC: zoom=%f, speed=%i, screen=%ix%i, sv_x1=%f, sv_x2=%f, sv=%fx%f, sv_minlen=%f, screen_x_pre=%f, screen_y_pre=%f\n"
    spasc.zoom spasc.speed spasc.screen_width spasc.screen_height
    spasc.spaceview_x1 spasc.spaceview_y1
    spasc.spaceview_width spasc.spaceview_height spasc.spaceview_minlen
    spasc.screen_x_pre spasc.screen_y_pre;
  flush stderr
    
let spasc_recalc spasc =
  spasc.screen_minlen <-
    float_of_int (min spasc.screen_height spasc.screen_width);
  spasc.spaceview_minlen <- min spasc.spaceview_height spasc.spaceview_width;
  if spasc.screen_width > spasc.screen_height then begin
    spasc.screen_x_pre <- 0.0;
    spasc.screen_y_pre <-
      (float_of_int (spasc.screen_height - spasc.screen_width)) /. 2.0
  end else begin
    spasc.screen_x_pre <-
      (float_of_int (spasc.screen_width - spasc.screen_height)) /. 2.0;
    spasc.screen_y_pre <- 0.0
  end

let prev_screen_height = ref 0
let prev_screen_width = ref 0


let resize_screen spasc height width =
  if (height <> !prev_screen_height) or (width <> !prev_screen_width) then begin
    Printf.fprintf stderr "resize_screen\n"; flush stderr;
    spasc.screen_height <- height; 
    spasc.screen_width <- width;
    spasc_recalc spasc
  end

let ccx spasc coord =
  ((coord -. spasc.spaceview_x1) *. (spasc.screen_minlen -. 1.0)) /.
    spasc.spaceview_minlen -. spasc.screen_x_pre

let ccy spasc coord =
  ((coord -. spasc.spaceview_y1) *. (spasc.screen_minlen -. 1.0)) /.
    spasc.spaceview_minlen -. spasc.screen_y_pre


(* convert a value (radius, ..)
*)
let vc spasc value =
  value *. spasc.screen_minlen /. spasc.spaceview_minlen

let surface_from_gdk_pixmap gdkpixmap =
  Cairo_lablgtk.create gdkpixmap

let set_color surface (r,g,b) =
  Cairo.set_source_rgb surface r g b

let paint_trace ?(color=rgb_cyan) surface points =
  let rec worker = function
    [] ->
      Cairo.stroke surface
    | (x,y) :: ps ->
	Cairo.line_to surface x y;
	worker ps
  in
    match points with
	[] -> ()
      | (x,y) :: ps ->
	  set_color surface color;
	  Cairo.move_to surface x y;
	  worker ps;
	  Cairo.stroke surface

let paint_line surface x1 y1 x2 y2 =
  Cairo.move_to surface x1 y1;
  Cairo.line_to surface x2 y2;
  Cairo.stroke surface

let paint_filled_circle surface spasc x y r =
  Cairo.save surface;
  Cairo.new_path surface;
  Cairo.arc surface x  y r 0. two_pi;
  Cairo.fill surface;
  Cairo.restore surface;
  Cairo.stroke surface

let paint_circle surface spasc x y r =
  Cairo.arc surface x y r 0. two_pi;
  Cairo.stroke surface

let show_trace ?(color=rgb_cyan) surface spasc points =
  paint_trace ~color surface
    (List.map (fun (x,y) -> (ccx spasc x), (ccy spasc y)) points)

let show_orbit ?(color=rgb_yellow) surface spasc r =
  set_color surface color;
  paint_circle surface spasc (ccx spasc 0.0) (ccy spasc 0.0) (vc spasc r)

let show_sat ?(color=rgb_red) surface spasc x y =
  set_color surface color;
  paint_circle surface spasc (ccx spasc x) (ccy spasc y) 3.0

let show_earth surface spasc =
  set_color surface rgb_green;
  Printf.fprintf stderr "painting earth to space %f, %f [%f] on tft %f, %f [%f]\n"
    0.0 0.0 earth_r (ccx spasc 0.0) (ccy spasc 0.0) (vc spasc earth_r);
  spasc_dump spasc;
  flush stderr;
  paint_filled_circle surface spasc (ccx spasc 0.0) (ccy spasc 0.0) (vc spasc earth_r)

let create_space surface spasc =
  show_earth surface spasc

let refresh_da da =
  GtkBase.Widget.queue_draw da#as_widget

let status_line = ref (GMisc.label ~text:"Statusline" ~justify:`FILL ())
    
let make_orbit_window () =
  let spasc = { zoom = earth_r *. initial_zoom;
		speed = initial_speed;
		screen_height = 500;
		screen_width = 500;
		screen_minlen = 500.0;
		screen_x_pre = 0.0;
		screen_y_pre = 0.0;
		spaceview_x1 = 0.0 -. earth_r *. initial_zoom;
		spaceview_y1 = 0.0 -. earth_r *. initial_zoom;
		spaceview_width = 2.0 *. earth_r *. initial_zoom;
		spaceview_height = 2.0 *. earth_r *. initial_zoom;
		spaceview_minlen = 2.0 *. earth_r *. initial_zoom;
	      }
  in let w = GWindow.window ~height:500 ~width:500 ()
  in let vbox = GPack.vbox ~packing:w#add ~homogeneous:false ()
  in let scrollwin = GBin.scrolled_window ~border_width:1 ~hpolicy:`AUTOMATIC
      ~vpolicy:`AUTOMATIC ~packing:(vbox#pack ~expand:true) ()
  in let da = GMisc.drawing_area ~packing:scrollwin#add ()
  in let hbox1 = GPack.hbox ~packing:(vbox#pack ~expand:false) ()
  in let bplay = GButton.button ~label:"Play" ~packing:hbox1#pack ()
  in let _ = GMisc.label ~text:"Zoom:"
      ~packing:(hbox1#pack ~expand:false) ()
  in let scrollx = GData.adjustment ~value:0.0
      ~lower:(0.0 -. initial_zoom *. earth_r) ~upper:(initial_zoom *. earth_r)
      ~step_incr:10.0 ~page_incr:earth_r ~page_size:1.0 ()
  in let scrolly = GData.adjustment ~value:0.0
      ~lower:(0.0 -. initial_zoom *. earth_r) ~upper:(initial_zoom *. earth_r)
      ~step_incr:1.0 ~page_incr:50.0 ~page_size:1.0 ()
  in let zoomer = GData.adjustment ~value:initial_zoom ~lower:1.0
      ~upper:100000.0 ~step_incr:1.0 ~page_incr:50.0 ~page_size:1.0 ()
  in let speeder = GData.adjustment ~value:(float_of_int initial_speed)
      ~lower:1.0 ~upper:10000.0 ~step_incr:1.0 ~page_incr:50.0 ~page_size:1.0 ()
  in let playing = ref false
  in
    scrollwin#set_hadjustment scrollx;
    scrollwin#set_vadjustment scrolly;
    ignore (zoomer#connect#value_changed
	      (fun () ->
		 spasc.zoom <- zoomer#value *. earth_r; refresh_da da));
    ignore (speeder#connect#value_changed
	      (fun () ->
		 spasc.speed <- int_of_float speeder#value));
    ignore (GEdit.spin_button ~adjustment:zoomer ~rate:0. ~digits:5 ~width:75 
	      ~packing:hbox1#pack ());
    ignore (GMisc.label ~text:"Speed:" ~packing:(hbox1#pack ~expand:false) ());
    ignore (GEdit.spin_button ~adjustment:speeder ~rate:0. ~digits:3 ~width:45 
	      ~packing:hbox1#pack ());
    hbox1#pack !status_line#coerce;
    da#misc#realize ();
    let q = if Array.length Sys.argv > 1 then
      Vmbridge.setup_file Sys.argv.(1)
    else
      failwith "biely mode not yet active.."
    in let d = new GDraw.drawable (da#misc#window)
    in let redraw_all _ =
      let da_width, da_height = Gdk.Drawable.get_size (da#misc#window)
      in let pixmap = GDraw.pixmap ~width:da_width ~height:da_height ()
      in
	resize_screen spasc da_width da_height;
	ignore (w#connect#destroy GMain.quit);
	pixmap#set_foreground (`NAME "blue");
	pixmap#rectangle ~x:0 ~y:0 ~width:da_width ~height:da_height
	  ~filled:true ();
	let surface = (surface_from_gdk_pixmap pixmap#pixmap)
	in
	  show_earth surface spasc;
	  show_orbit surface spasc !our_target_orbit;
	  show_sat surface spasc !our_x !our_y;
	  show_trace surface spasc !our_history;
	  d#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
	    ~width:da_width ~height:da_height pixmap#pixmap;
	  false
    in let remove_timeout = ref (fun () -> ())
    in let rec timeout_handler () =
	if !playing then begin
	  let stamp, score, fuel, x, y, orbit = q.Vmbridge.step spasc.speed;
	  in
	    if ((int_of_float !our_x) <> (int_of_float x)) or
	      ((int_of_float !our_y) <> (int_of_float y)) then begin
		if (!our_x <> 0.0) && (!our_y <> 0.0) then
		  our_history := (!our_x, !our_y) :: !our_history;
	      end;
	    our_x := x;
	    our_y := y;
	    our_target_orbit := orbit;
	    ignore (redraw_all ());
	    install_timeout_handler ();
	end;
	 false
       and install_timeout_handler () =
	let delta = 25
	in let toid = GMain.Timeout.add delta timeout_handler
	in
	  remove_timeout := (fun () -> GMain.Timeout.remove toid)
       and start_playing () =
	if not !playing then begin
	  playing := true;
	  bplay#set_label "Stop";
	  ignore (install_timeout_handler ())
	end
       and stop_playing () =
	if !playing then begin
	  bplay#set_label "Play";
	  playing := false;
	  !remove_timeout ()
	end
    in
      ignore (da#event#connect#expose ~callback:redraw_all);
      ignore (bplay#connect#clicked ~callback:
		(function () ->
		   if !playing then stop_playing () else start_playing ()));
      w#show ();
      GMain.Main.main ()

let _ =
  ignore (GMain.init ());
  make_orbit_window ()
