cmake_minimum_required(VERSION 2.4.6)
include($ENV{ROS_ROOT}/core/rosbuild/rosbuild.cmake)

rosbuild_init()
rosbuild_include(rosprolog utils)

rosbuild_find_ros_package(opencyc)
set( OPENCYC_PATH_PREFIX ${opencyc_PACKAGE_PATH} )
fix_ros_destination_path(OPENCYC_PATH_PREFIX)

file(GLOB inputs scripts/*.in)

foreach( _in ${inputs} )
  string(REPLACE ".in" "" _out ${_in})
  configure_file( ${_in} ${_out} )
endforeach( _in )

