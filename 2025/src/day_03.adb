with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Day_03 is
    function Bank_Max
       (Bank : Unbounded_String; Lights : Integer) return Big_Integer
    is
        I     : Positive;
        MAX_I : Positive;
        MAX_N : Integer := 0;
        R     : Big_Integer;
    begin
        if Lights = 0 then
            return 0;
        end if;

        for I in 1 .. Length (Bank) - Lights + 1 loop
            declare
                N : Integer :=
                   Character'Pos (Element (Bank, I)) - Character'Pos ('0');
            begin
                if N > MAX_N then
                    MAX_I := I;
                    MAX_N := N;
                end if;
                exit when MAX_N = 9;
            end;

        end loop;

        declare
            R : Big_Integer;
        begin
            R :=
               Bank_Max
                  (To_Unbounded_String
                      (Slice (Bank, MAX_I + 1, Length (Bank))),
                   Lights - 1);

            return
               To_Big_Integer (10)**(Lights - 1) * To_Big_Integer (MAX_N) + R;
        end;
    end;

    Line   : Unbounded_String;
    Silver : Big_Integer := 0;
    Gold   : Big_Integer := 0;
begin
    while not End_Of_File loop
        Line := To_Unbounded_String (Get_Line);
        Silver := Silver + Bank_Max (Line, 2);
        Gold := Gold + Bank_Max (Line, 12);
    end loop;

    Put_Line ("Silver: " & Silver'Image);
    Put_Line ("Gold: " & Gold'Image);
end;
