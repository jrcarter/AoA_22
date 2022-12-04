with Ada.Text_IO;

procedure Day02_2 is
   subtype Elf_Shape_ID is Character range 'A' .. 'C'; -- A => Rock, B => Paper, C => Scissors
   subtype My_Shape_ID  is Character range 'X' .. 'Z'; -- X     '    Y      "    Z       "
   subtype Shape_Value is Integer range 1 .. 3;
   subtype Score_Value is Integer range 0 .. 6 with Dynamic_Predicate => Score_Value in 0 | 3 | 6;

   Lose : constant Score_Value := 0;
   Draw : constant Score_Value := 3;
   Win  : constant Score_Value := 6;

   function Shape_Map (Elf_Shape : in Elf_Shape_ID; Match_Result : in My_Shape_ID) return Shape_Value;

   type Match_Score is array (My_Shape_ID) of Score_Value;

   function Shape_Map (Elf_Shape : in Elf_Shape_ID; Match_Result : in My_Shape_ID) return Shape_Value is
      -- Empty
   begin -- Shape_Map
      case Match_Result is
      when 'X' => -- Lose
         case Elf_Shape is
         when 'A' => -- Rock
            return 3; -- Scissors
         when 'B' => -- Paper
            return 1; -- Rock
         when 'C' => -- Scissors
            return 2; -- Paper;
         end case;
      when 'Y' => -- Draw
         case Elf_Shape is
         when 'A' => -- Rock
            return 1; -- Rock
         when 'B' => -- Paper
            return 2; -- Paper
         when 'C' => -- Scissors
            return 3; -- Scissors;
         end case;
      when 'Z' => -- Win
         case Elf_Shape is
         when 'A' => -- Rock
            return 2; -- Paper
         when 'B' => -- Paper
            return 3; -- Scissors
         when 'C' => -- Scissors
            return 1; -- Rock;
         end case;
      end case;
   end Shape_Map;

   Match_Map : constant Match_Score := ('X' => Lose, 'Y' => Draw, 'Z' => Win);

   Input : Ada.Text_IO.File_Type;
   Score : Natural := 0;
begin -- Day02_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_02");

   All_Matches : loop
      exit All_Matches when Ada.Text_IO.End_Of_File (Input);

      One_Match : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin -- One_Match
         Score := Score + Shape_Map (Line (Line'First), Line (Line'First + 2) ) + Match_Map (Line (Line'First + 2) );
      end One_Match;
   end loop All_Matches;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Score'Image);
end Day02_2;
