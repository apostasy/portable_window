with Interfaces.C; use Interfaces.C;



package Window is

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
   for color'Size use Interfaces.C.unsigned_long'size;

   procedure Window;

   procedure redraw;

   procedure Init (Width, Height : Interfaces.C.unsigned; Title : String);

   procedure draw_image_to_window (img : Image);

end Window;