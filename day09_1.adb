with Ada.Text_IO;

procedure Day09_1 is
   Max : constant := 1000; -- Size of grid

   subtype Index_Value is Integer range 1 .. Max;
   type Visit_Map is array (Index_Value, Index_Value) of Boolean;

   subtype Dir_Char is Character with Dynamic_Predicate => Dir_Char in 'U' | 'R' | 'D' | 'L';

   procedure Move (Dir     : in     Dir_Char;
                   Count   : in     Positive;
                   Hr      : in out Index_Value;
                   Hc      : in out Index_Value;
                   Tr      : in out Index_Value;
                   Tc      : in out Index_Value;
                   Visited : in out Visit_Map);
   -- Moves the head count spaces in direction Dir. updated Tr, Tc, and Visited for each step

   procedure Move (Dir     : in     Dir_Char;
                   Count   : in     Positive;
                   Hr      : in out Index_Value;
                   Hc      : in out Index_Value;
                   Tr      : in out Index_Value;
                   Tc      : in out Index_Value;
                   Visited : in out Visit_Map)
   is
      procedure Adjust (Hr : in Index_Value; Hc : in Index_Value; Tr : in out Index_Value; Tc : in out Index_Value);
      -- If Tail is not adjacent to Head, moves Tail one space according to the rules

      procedure Adjust (Hr : in Index_Value; Hc : in Index_Value; Tr : in out Index_Value; Tc : in out Index_Value) is
         -- Empty
      begin -- Adjust
         if Tr in Hr - 1 .. Hr + 1 and Tc in Hc - 1 .. Hc + 1 then -- Adjacent
            return;
         end if;

         if Tc < Hc then
            Tc := Tc + 1;
         elsif Tc > Hc then
            Tc := Tc - 1;
         else
            null;
         end if;

         if Tr < Hr then
            Tr := Tr + 1;
         elsif Tr > Hr then
            Tr := Tr - 1;
         else
            null;
         end if;
      end Adjust;

      Step : Integer := 1;
   begin -- Move
      if Dir in 'D' | 'L' then
         Step := -1;
      end if;

      case Dir is
      when 'U' | 'D' =>
         Vert : for I in 1 .. Count loop
            Hr := Hr + Step;
            Adjust (Hr => Hr, Hc => Hc, Tr => Tr, Tc => Tc);
            Visited (Tr, Tc) := True;
         end loop Vert;
      when 'R' | 'L' =>
         Horiz : for I in 1 .. Count loop
            Hc := Hc + Step;
            Adjust (Hr => Hr, Hc => Hc, Tr => Tr, Tc => Tc);
            Visited (Tr, Tc) := True;
         end loop Horiz;
      when others =>
         raise Program_Error with "Invalid direction " & Dir;
      end case;
   end Move;

   Input   : Ada.Text_IO.File_Type;
   Visited : Visit_Map   := (Max / 2 => (Max / 2 => True, others => False), others => (others => False) );
   Hr      : Index_Value := Max / 2;
   Hc      : Index_Value := Max / 2;
   Tr      : Index_Value := Max / 2;
   Tc      : Index_Value := Max / 2;
   Count   : Natural     := 0;
begin -- Day09_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_09");

   All_Moves : loop
      exit All_Moves when Ada.Text_IO.End_Of_File (Input);

      One_Move : declare
         Line  : constant String   := Ada.Text_IO.Get_Line (Input);
         Count : constant Positive := Integer'Value (Line (3 .. Line'Last) );
      begin -- One_Move
         Move (Dir => Line (1), Count => Count, Hr => Hr, Hc => Hc, Tr => Tr, Tc => Tc, Visited => Visited);
      end One_Move;
   end loop All_Moves;

   Ada.Text_IO.Close (File => Input);

   Count_Rows : for Row in Visited'Range (1) loop
      Count_Columns : for Column in Visited'Range (2) loop
         if Visited (Row, Column) then
            Count := Count + 1;
         end if;
      end loop Count_Columns;
   end loop Count_Rows;

   Ada.Text_IO.Put_Line (Item => Count'Image);
end Day09_1;
