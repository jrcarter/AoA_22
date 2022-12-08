with Ada.Text_IO;

procedure Day06_2 is
   function Is_Marker (Text : in String) return Boolean with Pre => Text'Length = 14;
   -- Returns True if Text is a marker; False otherwise
   -- A marker consists of Text'Length different Characters; a non-marker has at least one duplicated Character

   function Is_Marker (Text : in String) return Boolean is
      -- Empty
   begin -- Is_Marker
      All_Chars : for I in Text'First .. Text'Last - 1 loop
         All_Matches : for C of Text (I + 1 .. Text'Last) loop
            if Text (I) = C then
               return False;
            end if;
         end loop All_Matches;
      end loop All_Chars;

      return True;
   end Is_Marker;

   Input : Ada.Text_IO.File_Type;
begin -- Day06_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_06");

   Get_Line : declare
      Line : constant String := Ada.Text_IO.Get_Line (Input);
   begin -- Get_Line
      All_Markers : for I in 1 .. Line'Last - 13 loop
         if Is_Marker (Line (I .. I + 13) ) then
            Ada.Text_IO.Put_Line (Item => Integer'Image (I + 13) );

            exit All_Markers;
         end if;
      end loop All_Markers;
   end Get_Line;

   Ada.Text_IO.Close (File => Input);
end Day06_2;
