with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Day14_1 is
   subtype X_Value is Integer range 450 .. 520;
   subtype Y_Value is Integer range   0 .. 170;

   procedure Extract_Point (From : in String; X : out X_Value; Y : out Y_Value; Last : out Positive);
   -- Parses "mmm,nnn" from From, putting mmm in X and nnn in Y
   -- "nnn" is terminated by a space or the end of From
   -- Finished is True if "nnn" is terminated by the end of From; False if it's terminated by a space

   procedure Extract_Point (From : in String; X : out X_Value; Y : out Y_Value; Last : out Positive) is
      Stop  : Natural := Ada.Strings.Fixed.Index (From, ",") - 1;
      Start : Positive;
   begin -- Extract_Point
      X := Integer'Value (From (From'First .. Stop) );
      Start := Stop + 2;
      Stop := Ada.Strings.Fixed.Index (From (Start .. From'Last), " ");
      Stop := (if Stop = 0 then From'Last else Stop - 1);
      Last := Stop;
      Y := Integer'Value (From (Start .. Stop) );
   end Extract_Point;

   type Tile_ID is (Air, Rock, Sand);

   type Cave_Map is array (X_Value, Y_Value) of Tile_ID;

   Input : Ada.Text_IO.File_Type;
   Cave  : Cave_Map := (others => (others => Air) );
   X     : Positive;
   Y     : Natural;
   Count : Natural := 0;
begin -- Day14_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_14");

   All_Paths : loop
      exit All_Paths when Ada.Text_IO.End_Of_File (Input);

      One_Path : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);

         From_X   : X_Value;
         From_Y   : Y_Value;
         Last     : Positive;
         To_X     : X_Value;
         To_Y     : Y_Value;
      begin -- One_Path
         Extract_Point (From => Line, X => From_X, Y => From_Y, Last => Last);

         All_Lines : loop
            exit All_Lines when Last >= Line'Last;

            Extract_Point (From => Line (Last + 5 .. Line'Last), X => To_X, Y => To_Y, Last => Last);

            if From_X = To_X then -- Vertical
               Vert_Rock : for Y in Integer'Min (From_Y, To_Y) .. Integer'Max (From_Y, To_Y) loop
                  Cave (From_X, Y) := Rock;
               end loop Vert_Rock;
            else -- Horizontal
               Horiz_Rock : for X in Integer'Min (From_X, To_X) .. Integer'Max (From_X, To_X) loop
                  Cave (X, From_Y) := Rock;
               end loop Horiz_Rock;
            end if;

            From_X := To_X;
            From_Y := To_Y;
         end loop All_Lines;
      end One_Path;
   end loop All_Paths;

   Ada.Text_IO.Close (File => Input);

   All_Grains : loop
      X := 500;
      Y := 0;
      Count := Count + 1;
      --  Ada.Text_IO.Put_Line("grain"&Count'Image&" at"&X'Image&Y'Image);

      exit All_Grains when Cave (X, Y) /= Air;

      All_Motions : loop
      --  Ada.Text_IO.Put_Line("   falling");
         Fall : loop
            exit All_Grains when Y >= Y_Value'Last;
            exit Fall when Cave (X, Y + 1) /= Air;

            Y := Y + 1;
      --  Ada.Text_IO.Put_Line("      grain at"&X'Image&Y'Image);
         end loop Fall;
         --  Ada.Text_IO.Put_Line("   fall ended");

         if X > X_Value'First and then Cave (X - 1, Y + 1) = Air then
            X := X - 1;
            Y := Y + 1;
            --  Ada.Text_IO.Put_Line("   moved left"&X'Image&Y'Image);
         elsif X < X_Value'Last and then Cave (X + 1, Y + 1) = Air then
            X := X + 1;
            Y := Y + 1;
            --  Ada.Text_IO.Put_Line("   moved right"&X'Image&Y'Image);
         else
         --  Ada.Text_IO.Put_Line("   motion ended");
            Cave (X, Y) := Sand;

            exit All_Motions;
         end if;
      end loop All_Motions;
   end loop All_Grains;

   Ada.Text_IO.Put_Line (Item => Integer'Image (Count - 1) );

   for Y in Cave'Range (2) loop
   for X in Cave'Range (1) loop
   case Cave(X,Y) is
   when Air =>
   Ada.Text_IO.Put('.');
   when Rock =>
   Ada.Text_IO.Put('#');
   when Sand =>
   Ada.Text_IO.Put('o');
   end case;
   end loop;
   Ada.Text_IO.New_Line;
   end loop;
end Day14_1;
