with Ada.Containers.Ordered_Sets;
with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Day15_1 is
   subtype X_Value is Integer range -2_000_000 .. 5_000_000;
   subtype Y_Value is Integer range          0 .. 5_000_000;

   type Point_Value is record
      X : X_Value;
      Y : Y_Value;
   end record;

   function "<" (Left : in Point_Value; Right : in Point_Value) return Boolean is
      (if Left.X = Right.X then Left.Y < Right.Y else Left.X < Right.X);

   package Point_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Point_Value);

   type Sensor_Info is record
      Point : Point_Value;
      Dist  : Natural;
   end record;

   function "<" (Left : in Sensor_Info; Right : in Sensor_Info) return Boolean is
      (if Left.Point = Right.Point then Left.Dist < Right.Dist else Left.Point < Right.Point);

   package Sensor_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Sensor_Info);

   function Distance (From : in Point_Value; To : in Point_Value) return Natural is
      (abs (From.X - To.X) + abs (From.Y - To.Y) );
   -- Returns the Manhattan distance between From and To

   procedure Check_One (Position : in Sensor_Sets.Cursor);
   -- Determines points in Row that cannot have a beacon based on the sensor referenced by Position & adds them to Known_Not

   Row : constant := 2_000_000;

   Known     : Point_Sets.Set;
   Known_Not : Point_Sets.Set;

   procedure Check_One (Position : in Sensor_Sets.Cursor) is
      Sensor : constant Sensor_Info := Sensor_Sets.Element (Position);

      Point : Point_Value := (X => 0, Y => Row);
   begin -- Check_One
      if Distance ( (X => Sensor.Point.X, Y => Row), Sensor.Point) > Sensor.Dist then -- Sensor's exclusion zone can't include Row
         return;
      end if;

      -- Avoid iterating over all of X_Value
      Check_Left : for X in reverse X_Value'First .. Sensor.Point.X loop
         Point.X := X;

         exit Check_Left when Distance (Point, Sensor.Point) > Sensor.Dist;

         if not Known.Contains (Item => Point) then -- This is not the location of a known beacon
            Known_Not.Include (New_Item => Point); -- This is a location known not to be a beac0n
         end if;
      end loop Check_Left;

      Check_Right : for X in Sensor.Point.X + 1 .. X_Value'Last loop
         Point.X := X;

         exit Check_Right when Distance (Point, Sensor.Point) > Sensor.Dist;

         if not Known.Contains (Item => Point) then -- This is not the location of a known beacon
            Known_Not.Include (New_Item => Point); -- This is a location known not to be a beac0n
         end if;
      end loop Check_Right;
   end Check_One;

   Input   : Ada.Text_IO.File_Type;
   Sensors : Sensor_Sets.Set;
begin -- Day15_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_15");

   All_PairS : loop
      exit All_Pairs when Ada.Text_IO.End_Of_File (Input);

      One_Pair : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);

         Start  : Positive := Ada.Strings.Fixed.Index (Line, "=") + 1;
         Stop   : Positive := Ada.Strings.Fixed.Index (Line (Start .. Line'Last), ",") - 1;
         Sensor : Point_Value;
         Beacon : Point_Value;
         Dist   : Natural;
      begin -- One_Pair
         Sensor.X := Integer'Value (Line (Start .. Stop) );
         Start := Ada.Strings.Fixed.Index (Line (Stop + 1 .. Line'Last), "=") + 1;
         Stop := Ada.Strings.Fixed.Index (Line (Start .. Line'Last), ":") - 1;
         Sensor.Y := Integer'Value (Line (Start .. Stop) );
         Start := Ada.Strings.Fixed.Index (Line (Stop + 1 .. Line'Last), "=") + 1;
         Stop := Ada.Strings.Fixed.Index (Line (Start .. Line'Last), ",") - 1;
         Beacon.X := Integer'Value (Line (Start .. Stop) );
         Start := Ada.Strings.Fixed.Index (Line (Stop + 1 .. Line'Last), "=") + 1;
         Beacon.Y := Integer'Value (Line (Start .. Line'Last) );
         Dist := Distance (Sensor, Beacon);
         Sensors.Include (New_Item => (Point => Sensor, Dist => Dist) );

         if Sensor.Y = Row then -- Sensor is a point in Row that we know is not a beacon
            Known_Not.Include (New_Item => Sensor);
         end if;

         if Beacon.Y = Row then -- Beacon is a point in Row that we know is a beacon
            Known.Include (New_Item => Beacon);
         end if;
      end One_Pair;
   end loop All_Pairs;

   Ada.Text_IO.Close (File => Input);
   Sensors.Iterate (Process => Check_One'Access);
   Ada.Text_IO.Put_Line (Item => Known_Not.Length'Image);
end Day15_1;
