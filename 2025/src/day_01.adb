with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada;

procedure Day_01 is
    type Direction is (L, R);

    Dial_Mod : constant Big_Integer := 100;
    Dial_Pos : Big_Integer := 50;
    Rot_Line : Unbounded_String;
    Rot_Dir : Direction;
    Rot_Num : Big_Integer;

    Silver_Count : Big_Integer := 0;
    Gold_Count : Big_Integer := 0;
begin
    while not Text_IO.End_Of_File loop
        Rot_Line := To_Unbounded_String(
            Text_IO.Get_Line
        );
        Rot_Dir := Direction'Value(
            Slice(Rot_Line, 1, 1)
        );
        Rot_Num := From_String(
            Slice(Rot_Line, 2, Length(Rot_Line))
        );

        declare
            Dial_New_Pos : Big_Integer;
        begin
            Dial_New_Pos := Dial_Pos + (
                case Rot_Dir is
                    when L => - Rot_Num,
                    when R => + Rot_Num
            );

            Gold_Count := Gold_Count + abs(Dial_New_Pos) / Dial_Mod;
            if Dial_Pos /= 0 and then Dial_New_Pos <= 0 then
                Gold_Count := Gold_Count + 1;
            end if;

            Dial_Pos := Dial_New_Pos mod Dial_Mod;
        end;

        if Dial_Pos = 0 then
            Silver_Count := Silver_Count + 1;
        end if;
    end loop;

    Text_IO.Put_Line("Silver: " & Silver_Count'Image);
    Text_IO.Put_Line("Gold: " & Gold_Count'Image);
end;
