with Ada.Text_IO;

procedure Day02_1 is
   subtype Elf_Shape_ID is Character range 'A' .. 'C'; -- A => Rock, B => Paper, C => Scissors
   subtype My_Shape_ID  is Character range 'X' .. 'Z'; -- X     '    Y      "    Z       "
   subtype Shape_Value is Integer range 1 .. 3;
   subtype Score_Value is Integer range 0 .. 6 with Dynamic_Predicate => Score_Value in 0 | 3 | 6;

   Lose : constant Score_Value := 0;
   Draw : constant Score_Value := 3;
   Win  : constant Score_Value := 6;

   type Shape_Score is array (My_Shape_ID) of Shape_Value;
   type Match_Score is array (Elf_Shape_ID, My_Shape_ID) of Score_Value;

   Shape_Map : constant Shape_Score := ('X' => 1, 'Y' => 2, 'Z' => 3);
   Match_Map : constant Match_Score := ('A' => ('X' => Draw, 'Y' => Win,  'Z' => Lose),
                                        'B' => ('X' => Lose, 'Y' => Draw, 'Z' => Win),
                                        'C' => ('X' => Win,  'Y' => Lose, 'Z' => Draw) );
   Input : Ada.Text_IO.File_Type;
   Score : Natural := 0;
begin -- Day02_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_02");

   All_Matches : loop
      exit All_Matches when Ada.Text_IO.End_Of_File (Input);

      One_Match : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin -- One_Match
         Score := Score + Shape_Map (Line (Line'First + 2) ) + Match_Map (Line (Line'First), Line (Line'First + 2) );
      end One_Match;
   end loop All_Matches;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Score'Image);
end Day02_1;
