
/** <module> knowrob_mud - An Implementation of a MUD server


@license LGPL
*/

:- set_prolog_flag(file_loading,verbose).
:- ensure_loaded('../../../../../src_lib/logicmoo_util/logicmoo_util_all.pl').
:- abolish(dbase_rosprolog,1).
:- dynamic(dbase_rosprolog/1). 
:- prolog_load_context(file, Here),asserta(dbase_rosprolog(Here)).
dbase_rosprolog_dir(DIR):- dbase_rosprolog(Here),absolute_file_name(Here,AHere),file_directory_name(AHere,DIR).

user:file_search_path(knowrob,Where):- dbase_rosprolog(Here),file_directory_name(Here,DIR),concat_paths(DIR,'../../../*/',OOO),enumerate_files(OOO,Where).
user:file_search_path(pack, knowrob).
user:file_search_path(ros, knowrob).

% :-asserta((library_directory(Where):-ros_library_directory(Where))).
user:library_directory(Where):-ros_library_directory(Where).

:- dynamic(ros_package_initialized/1).

% enumerate_files(Spec,Result):- \+ exists_file_or_dir(Spec),!,expand_file_search_path(Spec,Result).
enumerate_files(Spec,Result):-expand_file_name(Spec,ResultList),member(NResult,ResultList),normalize_path(NResult,Result).

absolute_file_name_or_dir_opts(Spec,File,Options):-absolute_file_name(Spec,File,[file_type(directory),file_errors(fail)|Options])*->true;absolute_file_name(Spec,File,Options).

absolute_file_name_or_dir(Spec,File):-absolute_file_name(Spec,File,[file_type(directory),file_errors(fail)])*->true;absolute_file_name(Spec,File).

exists_file_or_dir(X):-exists_file(X),!.
exists_file_or_dir(X):-is_directory(X).
is_directory(X):-exists_directory(X).

concat_paths(ParentIn,Child,Result):- enumerate_files(ParentIn,Parent),
   (is_directory(Parent) -> directory_file_path(Parent,Child,Joined) ; atom_concat(Parent,Child,Joined)),
   enumerate_files(Joined,Result).

concat_paths([Joined],Result):- !,enumerate_files(Joined,Result).
concat_paths([ParentIn,Child|MORE],Result):- concat_paths(ParentIn,Child,ResultM),concat_paths([ResultM|MORE],Result).


ros_library_directory(WhereF2):-user:file_search_path(knowrob,W),once((atom_concat(W,'/*/prolog/',O),
  expand_file_name(O,ListE))),member(Where,ListE),normalize_path(Where,WhereF2).

normalize_path(Where,WhereF2):- absolute_file_name(Where,WhereF),prolog_to_os_filename(WhereF,WhereF1),prolog_to_os_filename(WhereF2,WhereF1),!.
normalize_path(Where,Where):-!.

ros_package(B):-ros_library_directory(WhereF2),concat_paths(WhereF2,(..),O),file_base_name(O,B).

ros_package_initfile(PackagePath,InitFile):-
  concat_paths([PackagePath,'./prolog/init{.pl,.pl.in,}'],Where),expand_file_name(Where,[InitFile|_]),!.
ros_package_initfile(RosPack,InitFile):-
  expand_file_search_path(knowrob(RosPack/prolog/'init{.pl,.pl.in,}'),Where),expand_file_name(Where,[InitFile|_]).

ros_package_dir(RosPack,Dir):- ros_package_initfile(RosPack,InitFile),concat_paths([InitFile,'./../..'],Dir).



%% rospack_package_path(+Package, -Path) is nondet.
%
% Locate ROS packages on the harddisk using 'rospack find'
% 
% @param Package  Name of a ROS package
% @param Path     Global path of Package determined by 'rospack find'
% 
rospack_package_path(Package, Path) :- ros_package_dir(Package, Path),must(exists_directory(Path)).
rospack_package_path(Package, Path) :- expand_file_search_path(knowrob(Package),Path),exists_directory(Path).
rospack_package_path(Package, Path) :- must(rospack_find_package_path(Package, Path)).
rospack_find_package_path(Package, Path) :-
  nonvar(Package),
  process_create(path('rospack'), ['find', Package], [stdout(pipe(RospackOutput)), process(PID)]),
  read_line_to_codes(RospackOutput, C),
  string_to_list(Path, C),
  process_wait(PID, _).

%% init_ros_package(+PackagePath) is nondet.
%
% Initialize a KnowRob package by consulting the prolog/init.pl file
% 
% @param PackagePath  Path towards the package to be initialized
% 
init_ros_package( Package ) :- do_not_register_ros_package( Package )  ,!.
init_ros_package( PackagePath ) :-
  (ros_package_initfile(PackagePath,InitFile);atom_concat(PackagePath, 'init.pl', InitFile)),
  exists_file(InitFile),
  must(consult(InitFile)), !.

init_ros_package( _ ).

%% register_ros_package(+Package) is nondet.
%% register_ros_package(+Package, ?AbsoluteDirectory) is nondet.
%
% Find and initialize a KnowRob package, i.e. locate the package on the
% harddisk, add the path to the library search path, and consult the init.pl
% file that (recursively) initializes the package and its dependencies.
% 
% @param Package            Name of a ROS package
% @param AbsoluteDirectory  Global path to the 'prolog' subdirectory in the given package
% 

do_not_register_ros_package( rosprolog ) :-!.
% do_not_register_ros_package( semweb ) :-!.
% do_not_register_ros_package( jpl ) :-!.
% do_not_register_ros_package( thea ) :-!.

register_ros_package( Package, _ ) :- do_not_register_ros_package( Package )  ,!.

register_ros_package(Package, _) :-
  current_predicate(ros_package_initialized/1),
  ros_package_initialized(Package), !.

register_ros_package(Package, AbsoluteDirectory) :-
  rospack_package_path(Package, PackagePath),
  nonvar(PackagePath),
  atom_concat(PackagePath, '/prolog/', AbsoluteDirectory),
  asserta(library_directory(AbsoluteDirectory)),
  assert(user:file_search_path(ros, AbsoluteDirectory)),
  assert( ros_package_initialized(Package) ),
  add_ros_package_to_classpath(Package),
  init_ros_package( AbsoluteDirectory ).


register_ros_package(Package) :-
  register_ros_package(Package, _).

use_ros_module(Package, FilePath) :-
  register_ros_package(Package, AbsoluteDirectory),
  atom_concat(AbsoluteDirectory, FilePath, AbsoluteFilePath),
  use_module( AbsoluteFilePath ).



%% add_ros_package_to_classpath(+Package) is nondet.
% 
% Adds Java dependencies of Package to the CLASSPATH environment variable
%
% @param Package Name of a ROS package
% 
add_ros_package_to_classpath(Package):-
	rospack_package_classpath(Package, Path),
	atom_concat(':',Path,PackagePath),
	setenv("CLASSPATH",PackagePath).

%% rospack_package_classpath(+Package, -Path) is nondet.
% 
% Calculates the Java dependencies of Package and returns a string to be appended to the CLASSPATH
%
% @param Package  Name of a ROS package
% @param Path     String with the dependencies to be added to the CLASSPATH
% 
% calculates java dependencies for classpath
rospack_package_classpath(Package, Path) :- ros_package_dir(Package, PackPath),atom_concat(PackPath,'**/*.jar',O),!,
  expand_file_name(O,ListE),atomic_list_concat(ListE,':',Path).
rospack_package_classpath(Package, Path) :-
  nonvar(Package),
  process_create(path('rosrun'), ['rosprolog', 'get_pkg_classpath', Package], [stdout(pipe(RospackOutput)), process(PID)]),
  read_line_to_codes(RospackOutput, C),
  string_to_list(Path, C),
  process_wait(PID, _).


% concat a value to an environment varible
% please note: delimiters have to be set within Val, e.g.:
% ':/path/to/lib:/path/to/lib2'
concat_env(Var,Val):-
	(getenv(Var,OldVal)
	->  (atom_concat(OldVal,Val,NewVal)) ;
        (NewVal = Val)),
	setenv(Var,NewVal).


:- register_ros_package(knowrob_roslog_launch).
:- register_ros_package(knowrob_cram).
:- register_ros_package(knowrob_sim).
:- register_ros_package(knowrob_mongo).
:- register_ros_package(knowrob_vis).
:- register_ros_package(iai_semantic_maps).
:- register_ros_package(knowrob_srdl).
:- show_call(forall(user:file_search_path(knowrob,_Where),true)).


end_of_file.

:- init_ros_package(knowrob_common).
% :- init_ros_package(prolog_perception).
% :- init_ros_package(knowrob_actions).
% :- init_ros_package(ias_knowledge_base).
% :- init_ros_package(comp_missingobj).
% :- init_ros_package(comp_semantic_map).
% :- init_ros_package(semweb).
:- forall(ros_package(B),init_ros_package(B)).









