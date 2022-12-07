with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with PragmARC.Data_Structures.Stacks.Unbounded.Unprotected;

procedure Day05_1 is
   subtype Crate_ID is Character range 'A' .. 'Z';

   package Line_Stacks is new
      PragmARC.Data_Structures.Stacks.Unbounded.Unprotected (Element => Ada.Strings.Unbounded.Unbounded_String);

   package Crate_Stacks is new PragmARC.Data_Structures.Stacks.Unbounded.Unprotected (Element => Crate_ID);

   subtype Stack_ID is Integer range 1 .. 9;

   type Stack_Map is array (Stack_ID) of Crate_Stacks.Handle;

   Input       : Ada.Text_IO.File_Type;
   Line_Stack  : Line_Stacks.Handle;
   Crate_Stack : Stack_Map;
   Dummy       : Ada.Strings.Unbounded.Unbounded_String;
   ID          : Crate_ID;

   use Ada.Strings.Unbounded;
begin -- Day05_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_05");

   All_Stacks : loop
      One_Stack : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin -- One_Stack
         exit All_Stacks when Line = "";

         Line_Stack.Push (Item => To_Unbounded_String (Line) );
      end One_Stack;
   end loop All_Stacks;

   Line_Stack.Pop (Item => Dummy); -- Discared line with stack IDs

   Fill_Stacks : loop
      exit Fill_Stacks when Line_Stack.Is_Empty;

      One_Set : declare
         Line  : Unbounded_String;
         Index : Positive;
      begin -- One_Set
         Line_Stack.Pop (Item => Line);

         Stack_Contents : for I in Crate_Stack'Range loop
            Index := (if I = Crate_Stack'First then 2 else Index + 4);

            if Element (Line, Index) /= ' ' then
               Crate_Stack (I).Push (Item => Element (Line, Index) );
            end if;
         end loop Stack_Contents;
      end One_Set;
   end loop Fill_Stacks;

   All_Moves : loop
      exit All_Moves when Ada.Text_IO.End_Of_File (Input);

      One_Move : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);

         Start : Natural;
         Stop  : Natural;
         Count : Positive;
         From  : Stack_ID;
         To    : Stack_ID;
      begin -- One_Move
         Start := 6; -- Skip "move "
         Stop := Ada.Strings.Fixed.Index (Line (Start .. Line'Last), " ") - 1; -- Blank after count
         Count := Integer'Value (Line (Start .. Stop) );
         Start := Stop + 7; -- Skip " from "
         From := Integer'Value (Line (Start .. Start) ); -- IDs are always 1 character
         Start := Start + 5; -- Skip " to "
         To := Integer'Value (Line (Start .. Start) );

         All_Crates : for I in 1 .. Count loop
            Crate_Stack (From).Pop (Item => ID);
            Crate_Stack (To).Push (Item => ID);
         end loop All_Crates;
      end One_Move;
   end loop All_Moves;

   Ada.Text_IO.Close (File => Input);

   Output : for I in Crate_Stack'Range loop
      Crate_Stack (I).Pop (Item => ID);
      Ada.Text_IO.Put (Item => ID);
   end loop Output;

   Ada.Text_IO.New_Line;
end Day05_1;
