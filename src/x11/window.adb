with Interfaces.C.Strings;
with Interfaces;
with System;

package body Window is

   package ICS renames Interfaces.C.Strings;
   package IC renames interfaces.C;

   D : constant := 1.0 / 256.0;
  type intensity is delta D range 0.0 .. 1.0 with size => 8;
  function Max return intensity is
      (1.0 - D);

  -- Note that Max_Lines and Max_Length need to be static
    type color_data is array (positive range <>, positive range <>) of intensity;

    type image (width : natural := 0; height : natural := 0) is
    record
        r : color_data (1 .. width, 1 .. height);
        g : color_data (1 .. width, 1 .. height);
        b : color_data (1 .. width, 1 .. height);
        a : color_data (1 .. width, 1 .. height);
    end record;

   type bool is new boolean;
   for bool'size use 8;
   type color is record
      r : intensity;
      g : intensity;
      b : intensity;
      a : intensity;
   end record;
   for color use
     record
       b at 0 range 0 .. 7;
       g at 1 range 0 .. 7;
       r at 2 range 0 .. 7;
       a at 3 range 0 .. 7;
     end record;
   for color'Size use ic.unsigned_long'size;

   Exposure_Mask         : constant := 32768;
   Expose                : constant := 12;

   type X_Display_Access is new System.Address;
   type X_Window is new IC.Unsigned_Long;

   type X_Expose_Event is record
         W      : X_Window;                      
         Xx     : IC.int;
         Y      : IC.int;
         Width  : IC.int;
         Height : IC.int;
         Count  : IC.int;
   end record;
   
   type X_Event is record 
        Event_Type   : IC.int;                        
        Serial       : IC.unsigned_long;                     
        Send_Event   : Boolean;                                
        Display      : X_Display_Access;                   
        Expose       : X_Expose_Event;
    end record;
   type X_Event_Access is access all X_Event;

   
   type X_Drawable is new IC.Unsigned_Long;
   type X_Graphic_Context is new System.Address;
   type X_Graphic_Context_Access is access all X_Graphic_Context;

   type Const_Char_Access is access constant IC.char;

  procedure X_Set_Foreground
     (display    : X_Display_Access;
      gc         : X_Graphic_Context_Access;
      foreground : ic.unsigned_long)
   with Import => True, Convention => c, External_Name => "XSetForeground";
     
  procedure X_Draw_Point
     (display : X_Display_Access;
      w       : X_Window;
      gc      : X_Graphic_Context_Access;
      x       : ic.int;
      y       : ic.int)
   with Import => True, Convention => c, External_Name => "XDrawPoint";

   function X_Open_Display
     (Display_Name : ICS.Chars_Ptr) return X_Display_Access
   with Import => True, Convention => C, External_Name => "XOpenDisplay";

   function X_Default_Screen (Display : X_Display_Access) return IC.Int
   with Import => True, Convention => C, External_Name => "XDefaultScreen";

   function X_Default_Root_Window (Display : X_Display_Access) return X_Window
   with Import => True, Convention => C, External_Name => "XDefaultRootWindow";

   function X_Black_Pixel
     (Display : X_Display_Access; Screen_Number : IC.Int) return IC.Unsigned_Long
   with Import => True, Convention => C, External_Name => "XBlackPixel";

   function X_White_Pixel
     (Display : X_Display_Access; Screen_Number : IC.Int) return IC.Unsigned_Long
   with Import => True, Convention => C, External_Name => "XWhitePixel";

   function X_Create_Simple_Window
     (Display      : X_Display_Access;
      Parent       : X_Window;
      X            : IC.Int;
      Y            : IC.Int;
      Width        : IC.Unsigned;
      Height       : IC.Unsigned;
      Border_Width : IC.Unsigned;
      Border       : IC.Unsigned_Long;
      Background   : IC.Unsigned_Long) return X_Window
   with
     Import => True,
     Convention => c,
     External_Name => "XCreateSimpleWindow";

   procedure X_Map_Window (Display : X_Display_Access; W : X_Window)
   with Import => True, Convention => C, External_Name => "XMapWindow";

   procedure X_Select_Input
     (Display : X_Display_Access; W : X_Window; Event_Mask : IC.Long)
   with Import => True, Convention => C, External_Name => "XSelectInput";

   procedure X_Next_Event
     (Display : X_Display_Access; Event_Return : X_Event_Access)
   with Import => True, Convention => c, External_Name => "XNextEvent";

   procedure X_Draw_String
     (Display : X_Display_Access;
      D       : X_Drawable;
      GC      : X_Graphic_Context_Access;
      XX      : IC.Int;
      Y       : IC.Int;
      String  : IC.Char_Array;
      Length  : IC.Int)
   with Import => True, Convention => C, External_Name => "XDrawString";

   function X_Default_Graphic_Context
     (Display : X_Display_Access; Screen_Number : IC.Int)
      return X_Graphic_Context_Access
   with Import => True, Convention => C, External_Name => "XDefaultGC";


  Display    : X_Display_Access := X_Open_Display (ICS.Null_Ptr);
  Screen_num : IC.Int := X_Default_Screen (Display);
  Win        : X_Window :=
    X_Create_Simple_Window
      (Display,
        X_Default_Root_Window (Display),
        50,
        50,
        250,
        250,
        1,
        X_Black_Pixel (Display, Screen_num),
        X_White_Pixel (Display, Screen_num));

   procedure draw_image_to_window (img : Image) is
      use ic;

      procedure draw_pixel (x : ic.int; y : ic.int; color : ic.unsigned_long)
      is
      begin
         x_set_foreground (Display, X_Default_Graphic_Context (Display, 0), color);
         x_draw_point (Display, win, X_Default_Graphic_Context (Display, 0), x, y);
      end;

      x0, y0 : ic.int := 0;

   begin
      for i in img.r'range(1) loop
         for j in img.r'range(2) loop
            declare
               c       : color :=
                 (img.r (i, j),
                  img.g (i, j),
                  img.b (i, j),
                  img.a (i, j));
               c_color : ic.unsigned_long;
               for c_color'address use c'address;
            begin
               if c_color > 0 then
                  draw_pixel (ic.int (i), ic.int (j), c_color);
               end if;
            end;
         end loop;
      end loop;
   end;

   procedure Window is
      Event_Ptr  : X_Event_Access := new X_Event;
      c : color;
   begin
      X_Map_Window (Display, Win);
      X_Select_Input (Display, Win, Exposure_Mask);
      loop
         X_Next_Event (Display, Event_Ptr);
         case Event_Ptr.all.Event_Type is
            when Expose =>
               X_Draw_String
                 (Display,
                  X_Drawable (Win),
                  X_Default_Graphic_Context (Display, 0),
                  100,
                  100,
                  IC.To_C ("Thanks for Watching!"),
                  20);
                declare
                  c_color : ic.unsigned_long;
                  for c_color'address use c'address;
                begin
                  c := (Max, 0.0, 0.0, Max);
                  X_Set_Foreground (Display, X_Default_Graphic_Context (Display, 0), c_color);
                  X_Draw_Point (Display, Win, X_Default_Graphic_Context (Display, 0), 50, 50);
                  X_Draw_Point (Display, Win, X_Default_Graphic_Context (Display, 0), 50, 51);
                  X_Draw_Point (Display, Win, X_Default_Graphic_Context (Display, 0), 51, 50);
                  X_Draw_Point (Display, Win, X_Default_Graphic_Context (Display, 0), 51, 51);
                end;
            when others =>
               null;
         end case;
      end loop;
   end Window;
end Window;
