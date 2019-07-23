haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

not( X ) :-
   X,
   !,
   fail.

not( _ ).

getHours( time( Hours, Mins ), TotalHours ) :-
    TotalHours is Hours + Mins / 60.

doubleDigitize( Num ) :-
    Num < 10, 
    print( 0 ), 
    print( Num ).

doubleDigitize( Num ) :-
    Num >= 10, 
    print( Num ).

arrTime( Start, Dest, DTime, ATime) :-
   airport( Start, _, degmin(SNLatDeg, SNLatMin), 
            degmin(SWLonDeg, SWLonMin)),
   airport( Dest, _, degmin(DNLatDeg, DNLatMin), 
            degmin(DWLonDeg, DWLonMin)), 
   haversine_radians( ((SNLatDeg + (SNLatMin / 60)) * (pi / 180)), 
                      ((SWLonDeg + (SWLonMin / 60)) * (pi / 180)), 
                      ((DNLatDeg + (DNLatMin / 60)) * (pi / 180)), 
                      ((DWLonDeg + (DWLonMin / 60)) * (pi / 180)), 
                      Distance ),
   ATime is (Distance / 500) + DTime. 

findPath( AP1, AP1, _,[AP1], _ ).

findPath( AP1, AP2, Visited, [[AP1, DTime, ATime] | Path], DTimeHM) :-
   flight( AP1, AP2, DTimeHM ),
   not( member( AP2, Visited ) ),
   getHours( DTimeHM, DTime ),
   arrTime( AP1, AP2, DTime, ATime ),
   ATime < 24.0,
   findPath( AP2, AP2, [AP2 | Visited], Path, _ ).

findPath( AP1, AP2, Visited, [[AP1, DTime, ATime] | Path], DTimeHM) :-
   flight( AP1, H, DTimeHM ),
   not( member( H, Visited ) ),
   getHours( DTimeHM, DTime ),
   arrTime( AP1, H, DTime, ATime ),
   ATime < 24.0,
   flight( H, _, HDTimeHM ),
   getHours( HDTimeHM, HDtime ),
   (HDtime - ATime - 0.5) >= 0,
   findPath( H, AP2, [H | Visited], Path, HDTimeHM ). 

print_time( Hoursonly ) :-
    Hours is floor( Hoursonly ),
    Mins is ( (Hoursonly - Hours)*60 ),
    Temp is ( floor(Mins/ 60) ),
    Hours_updated is ( Temp + Hours ),
    Mins_updated is ( round(Mins - (Temp*60)) ),
    print_nums( Hours_updated ),
    print( ':' ),
    print_nums( Mins_updated ).

writePath( [] ) :-
   nl.

writePath( [[AP1, DTime, ATime], AP2 | []] ) :-
   airport( AP1, AP1City, _, _ ),
   airport( AP2, AP2City, _, _ ),
   DHours is floor( DTime ),
   DMins is ( round( (DTime - DHours)*60 ) ),
   AHours is floor( ATime ),
   AMins is ( round( (ATime - AHours)*60 ) ),
   format( '     depart  ~w  ~w',[AP1, AP1City] ),
   doubleDigitize( DHours ),
   write( ':' ),
   doubleDigitize( DMins ),
   format( '~n     arrive  ~w  ~w',[AP2, AP2City] ),
   doubleDigitize( AHours ),
   write( ':' ),
   doubleDigitize( AMins ),
   nl,
   !,
   true.

writePath( [[AP1, AP1DTime, HAPATime], 
            [HAP, HAPDTime, AP2ATime] | PathLeft] ) :-
   airport( AP1, AP1City, _, _ ),
   airport( HAP, HAPCity, _, _ ),
   AP1DHours is floor( AP1DTime ),
   AP1DMins is ( round( (AP1DTime - AP1DHours)*60 ) ),
   HAPAHours is floor( HAPATime ),
   HAPAMins is ( round( (HAPATime - HAPAHours)*60 ) ),
   format( '     depart  ~w  ~w',[AP1, AP1City] ),
   doubleDigitize( AP1DHours ),
   write( ':' ),
   doubleDigitize( AP1DMins ),
   format(' ~n     arrive  ~w  ~w',[HAP, HAPCity] ),
   doubleDigitize( HAPAHours ),
   write( ':' ),
   doubleDigitize( HAPAMins ),
   nl,
   !,
   writePath( [[HAP, HAPDTime, AP2ATime] | PathLeft] ).

fly( AP, AP ) :-
   write('You\'re already at your destination'), 
   nl, 
   !,
   fail.

fly( AP1, AP2 ) :-
   airport(AP1, _, _, _ ),
   airport(AP2, _, _, _ ),
   findPath( AP1, AP2, [AP1], Path, _ ),
   nl, 
   !,
   writePath(Path),
   true.

fly( AP1, _ ) :-
   not( airport(AP1, _, _, _ ) ),
   format('~w does not exist. ~n', [AP1]),
   !,
   fail.

fly( _, AP2 ) :-
   not( airport(AP2, _, _, _ ) ),
   format('~w does not exist. ~n', [AP2]),
   !,
   fail.

fly( AP1, AP2 ) :-
    airport( AP1, _, _, _ ),
    airport( AP2, _, _, _ ),
    write( 'No Results Found' ),
    !, 
    fail.
