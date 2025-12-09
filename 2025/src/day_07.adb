with Ada.Containers.Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Containers;
use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Day_07 is
    Input_Line  : Unbounded_String;
    Input_First : Boolean := True;

    package T_Vec is new
       Vectors (Index_Type => Positive, Element_Type => Big_Integer);
    use T_Vec;

    Tachyons     : T_Vec.Vector;
    Tachyons_New : T_Vec.Vector;

    Silver : Big_Integer := 0;
    Gold   : Big_Integer := 0;
begin
    while not End_Of_File loop
        Input_Line := To_Unbounded_String (Get_Line);
        if Input_First then
            Input_First := False;

            for Idx in 1 .. Length (Input_Line) loop
                case Element (Input_Line, Idx) is
                    when 'S'    =>
                        Append (Tachyons, 1);

                    when '.'    =>
                        Append (Tachyons, 0);

                    when others =>
                        raise Constraint_Error;
                end case;
            end loop;
        else
            Tachyons_New := Tachyons;

            for Idx in 1 .. Positive (T_Vec.Length (Tachyons)) loop
                Tachyons_New (Idx) := 0;
            end loop;

            for Idx in 1 .. Positive (Length (Input_Line)) loop
                if Tachyons (Idx) > 0 then
                    case Element (Input_Line, Idx) is
                        when '^'    =>
                            Silver := Silver + 1;
                            if Idx in 2 .. Positive (T_Vec.Length (Tachyons))
                            then
                                Tachyons_New (Idx - 1) :=
                                   Tachyons_New (Idx - 1) + Tachyons (Idx);
                            end if;
                            if Idx
                               in 1 .. Positive (T_Vec.Length (Tachyons)) - 1
                            then
                                Tachyons_New (Idx + 1) :=
                                   Tachyons_New (Idx + 1) + Tachyons (Idx);
                            end if;

                        when '.'    =>
                            Tachyons_New (Idx) :=
                               Tachyons_New (Idx) + Tachyons (Idx);

                        when others =>
                            raise Constraint_Error;
                    end case;
                end if;
            end loop;

            Tachyons := Tachyons_New;
        end if;
    end loop;

    Gold := 0;
    for Idx in 1 .. Positive (T_Vec.Length (Tachyons)) loop
        Gold := Gold + Tachyons (Idx);
    end loop;

    Put_Line ("Silver: " & Silver'Image);
    Put_Line ("Gold: " & Gold'Image);
end;
