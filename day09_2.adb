with Ada.Text_IO;

procedure Day09_2 is
   Max : constant := 1000; -- Size of grid

   subtype Index_Value is Integer range 1 .. Max;
   type Visit_Map is array (Index_Value, Index_Value) of Boolean;

   subtype Knot_ID is Integer range 0 .. 9;

   type Position is record
      Row    : Index_Value := Max / 2;
      Column : Index_Value := Max / 2;
   end record;

   type Position_Map is array (Knot_ID) of Position;

   subtype Dir_Char is Character with Dynamic_Predicate => Dir_Char in 'U' | 'R' | 'D' | 'L';

   procedure Move (Dir     : in     Dir_Char;
                   Count   : in     Positive;
                   Pos     : in out Position_Map;
                   Visited : in out Visit_Map);
   -- Moves knot 0 count spaces in direction Dir. updating the other knots and Visited for each step

   procedure Move (Dir     : in     Dir_Char;
                   Count   : in     Positive;
                   Pos     : in out Position_Map;
                   Visited : in out Visit_Map)
   is
      procedure Adjust (Head : in Position; Tail : in out Position);
      -- If Tail is not adjacent to Head, moves Tail one space according to the rules

      procedure Adjust (Head : in Position; Tail : in out Position) is
         -- Empty
      begin -- Adjust
         if Tail.Row in Head.Row - 1 .. Head.Row + 1 and Tail.Column in Head.Column - 1 .. Head.Column + 1 then -- Adjacent
            return;
         end if;

         if Tail.Column < Head.Column then
            Tail.Column := Tail.Column + 1;
         elsif Tail.Column > Head.Column then
            Tail.Column := Tail.Column - 1;
         else
            null;
         end if;

         if Tail.Row < Head.Row then
            Tail.Row := Tail.Row + 1;
         elsif Tail.Row > Head.Row then
            Tail.Row := Tail.Row - 1;
         else
            null;
         end if;
      end Adjust;

      Step : Integer := 1;
   begin -- Move
      if Dir in 'D' | 'L' then
         Step := -1;
      end if;

      All_Steps : for I in 1 .. Count loop
         case Dir is
         when 'U' | 'D' =>
            Pos (0).Row := Pos (0).Row + Step;
         when 'R' | 'L' =>
            Pos (0).Column := Pos (0).Column + Step;
         when others =>
            raise Program_Error with "Invalid direction " & Dir;
         end case;

         Move_Tail : for J in 0 .. 8 loop
            Adjust (Head => Pos (J), Tail => Pos (J + 1) );
         end loop Move_Tail;

         Visited (Pos (9).Row, Pos (9).Column) := True;
      end loop All_Steps;
   end Move;

   Input   : Ada.Text_IO.File_Type;
   Visited : Visit_Map := (Max / 2 => (Max / 2 => True, others => False), others => (others => False) );
   Knot    : Position_Map;
   Count   : Natural := 0;
begin -- Day09_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_09");

   All_Moves : loop
      exit All_Moves when Ada.Text_IO.End_Of_File (Input);

      One_Move : declare
         Line  : constant String   := Ada.Text_IO.Get_Line (Input);
         Count : constant Positive := Integer'Value (Line (3 .. Line'Last) );
      begin -- One_Move
         Move (Dir => Line (1), Count => Count, Pos => Knot, Visited => Visited);
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
end Day09_2;
