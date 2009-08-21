%%
%%  wpc_ambocc.erl --
%%
%%     Plug-in for Generating Ambient Occlusion via OpenGL
%%
%%     Uses hardware-accelerated OpenGL calls, instead of ray-tracing, to
%%     calculate and set a per-vertex ambient-occlusion factor. A HemiCube is
%%     used to sample the environment's visibility. The results are stored in
%%     the vertex-colors and can also be baked to a texture through autouv.
%%
%%     This module improves performance with newer cards requires shading 
%%     and fbo.
%%
%%  Copyright (c) 2009 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(ambocc_gl2).

-export([ambient_occlusion/1]).

-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").
-include("e3d_image.hrl").

-record(ao, {dl, sp, fbo, tex, cleanup_fbo}).
-define(SIZE, 64).
-define(RADIE, (?SIZE div 2)).
-define(AREA,  (?SIZE*?SIZE)).
-define(FISH_EYE_AREA, (3.141592653589793 * ?RADIE * ?RADIE)).

ambient_occlusion(St) ->
    StartTime = now(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    setup_gl(),
    AO_0 = setup_shaders(),
    DispList = make_disp_list(St),
    AO = AO_0#ao{dl=DispList},
    #st{shapes=Shapes} = St,
    ProcessObject = fun(_,We) -> process_obj(We,AO) end,
    Shapes2 = ?SLOW(gb_trees:map(ProcessObject, Shapes)),
    St2 = St#st{shapes=Shapes2},
    cleanup(AO),
    gl:popAttrib(),
    EndTime = now(),
    Seconds = timer:now_diff(EndTime,StartTime)/1.0e6,
    VidCard = gl:getString(?GL_RENDERER),
    io:fwrite("OpenGL AmbOcc time: ~.1fs (~s)\n", [Seconds,VidCard]),
    St3 = create_ambient_light(St2),
    St3.

setup_gl() ->
    gl:clearColor(1,1,1,0),  % Sky Color
    gl:color4f(0,0,0,1),     % Obj Color
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHTING).

setup_shaders() ->
    VS   = wings_gl:compile(vertex, fisheye_vs()),
    FS   = wings_gl:compile(fragment, fisheye_fs()),
    Prog = wings_gl:link_prog([VS,FS]),
    gl:useProgram(Prog),
    Buffers = wings_gl:setup_fbo({?SIZE,?SIZE}, 
				 [{color,[%{internal, ?GL_LUMINANCE8},
					  %{format,   ?GL_LUMINANCE},
					  {wrap_s,   ?GL_CLAMP_TO_EDGE},
					  {wrap_t,   ?GL_CLAMP_TO_EDGE}
					  %%,{min, ?GL_LINEAR_MIPMAP_LINEAR}
					  %%,{gen_mipmap, ?GL_TRUE}
					 ]}, 
				  {depth,[]}]),  
    [{fbo,Fbo},{color,Tex}|_] = Buffers,
    gl:drawBuffer(?GL_COLOR_ATTACHMENT0_EXT),
    #ao{sp=Prog, fbo=Fbo, tex=Tex, cleanup_fbo=Buffers}.

cleanup(#ao{sp=Shader, dl=DispList, cleanup_fbo=Fbo}) ->    
    gl:deleteLists(DispList,1),
    wings_gl:delete_fbo(Fbo),
    gl:useProgram(0),
    gl:deleteProgram(Shader).

process_obj(We, _) when ?IS_NOT_VISIBLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_NOT_SELECTABLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_ANY_LIGHT(We) ->
    case We#we.name =/= "Ambient" of
	true -> We#we{perm=[]};
	false -> We
    end;
process_obj(We0, AO) ->
    #we{es=Etab,vp=Vtab,name=Name} = We0,
    io:fwrite("Processing: ~s\n", [Name]),
    GetColor = fun(VertexId,_Val) ->
		       Eye = wings_vertex:pos(VertexId,We0) ,
		       LookAt = wings_vertex:normal(VertexId,We0),
		       get_ao_color(VertexId, Eye,LookAt,AO)
	       end,
    VertexColors = array:sparse_map(GetColor, Vtab),
    SetColor = fun(Edge, #edge{vs=Va,ve=Vb}, W) ->
		       Color1 = array:get(Va, VertexColors),
		       Color2 = array:get(Vb, VertexColors),
		       wings_va:set_edge_color(Edge, Color1, Color2, W)
	       end,
    array:sparse_foldl(SetColor, We0, Etab).

make_disp_list(St) ->
    #st{shapes=Shapes} = St,
    GetAllFaces = fun(_Key,Val) ->
			  Perm = Val#we.perm,
			  case ?IS_ANY_LIGHT(Val) or ?IS_NOT_VISIBLE(Perm) or ?IS_NOT_SELECTABLE(Perm) of
			      true ->
				  [];
			      false  ->
				  Fs = gb_trees:to_list(Val#we.fs),
				  [wings_face:vertex_positions(Face, Val) || {Face,_} <- Fs]
			  end
		  end,
    AddPolygons = fun(RawFs2) ->
			  ProcessVert = fun(Vert) ->
						{X,Y,Z} = Vert,
						gl:vertex3f(X,Y,Z)
					end,
			  ProcessFace = fun(Face) ->
						gl:'begin'(?GL_POLYGON),
						%%gl:'begin'(?GL_LINES),
						lists:foreach(ProcessVert, Face),
						gl:'end'()
					end,
			  lists:foreach(ProcessFace, RawFs2)
		  end,
    RawFs = gb_trees:map(GetAllFaces, Shapes),
    AllRawFs = lists:append(gb_trees:values(RawFs)),
    DispList = gl:genLists(1),
    gl:newList(DispList, ?GL_COMPILE),
    AddPolygons(AllRawFs),
    gl:endList(),
    DispList.

get_ao_color(VId, Eye, Lookat, AO) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    render_hemicube(Eye, Lookat, AO),
    Factor = read_frame(VId, AO),
    {Factor,Factor,Factor}.

read_frame(Vid, #ao{tex=Tex, fbo=Fbo}) ->
    Hemirez = ?SIZE, % Must be even and/or power-of-two
    W = H = Hemirez,
    Buffer = wings_io:get_buffer(W*H, ?GL_UNSIGNED_BYTE),
    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, 0),
    gl:bindTexture(?GL_TEXTURE_2D, Tex),
    %%
    %% Check if hardware mipmapping works on
    %% RGB textures, they don't work with LUMINANCE on ATI
    %% 
    %gl:generateMipmapEXT(?GL_TEXTURE_2D),
    gl:getTexImage(?GL_TEXTURE_2D, 0, ?GL_LUMINANCE, ?GL_UNSIGNED_BYTE, Buffer),
    ImageBin = wings_io:get_bin(Buffer),
    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, Fbo),
    %% Debug 
%%     case Vid of 
%% 	19 ->
%% 	    test_img("Vertex 19", ImageBin),
%% 	    AO = get_ao_factor(ImageBin),
%% 	    io:format("Vertex ~p: ~p~n", [Vid, AO]);
%% 	_ ->
%% 	    ok
%%     end,
    get_ao_factor(ImageBin).
%    <<Color:8, C1:8, C2:8, C3:8,_/binary>> = ImageBin,    
%    io:format("~p ~p => ~p~n", [Color,[C1,C2,C3],Color * ?FISH_EYE_AREA / ?AREA]),
%    erlang:min(1.0, Color/255 * ?AREA / ?FISH_EYE_AREA).

get_ao_factor(Buffer) ->
    Occluded = num_black(Buffer, 0),
    Result = 1.0 - (Occluded / (?FISH_EYE_AREA)),
    erlang:min(1.0, erlang:max(0.0, Result)).

num_black(<<255:8,Rest/binary>>, Sum) -> 
    num_black(Rest, Sum);
num_black(<<0:8, Rest/binary>>, Sum) ->
    num_black(Rest, Sum+1);
num_black(<<>>, Sum) ->
    Sum.

get_up_right(Lookat) ->
    Up1   = up(Lookat),
    Right = e3d_vec:cross(Up1, Lookat),
    Up    = e3d_vec:cross(Lookat, Right),
    {Up,Right}.

up({X,Y,Z}) 
  when abs(Y) > abs(X), abs(Y) > abs(Z) ->
    {1.0,0.0,0.0};
up(_) ->
    {0.0,1.0,0.0}.

model_view(Eye, {Dx,Dy,Dz}, {{UpX,UpY,UpZ}, {Rx,Ry,Rz}}) ->
    Z = 0.0, 
    Rot = {Rx, UpX, -Dx,
	   Ry, UpY, -Dy,
	   Rz, UpZ, -Dz,
	   Z,   Z,   Z},
    Trans = e3d_mat:translate(e3d_vec:neg(Eye)),
    e3d_mat:expand(e3d_mat:mul(Rot,Trans)).

render_hemicube(Eye, Lookat, DispList) ->
    Mat = model_view(Eye, Lookat, get_up_right(Lookat)),
    render_view(Mat, DispList).
render_view(Mat, #ao{dl=DispList}) ->
    W = H = ?SIZE,
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadMatrixd(Mat),
    gl:viewport(0,0,W,H),
    gl:callList(DispList).

create_ambient_light(St) ->
    wings_pref:set_value(scene_lights, true),
    SceneLights = wings_light:export(St),
    case proplists:is_defined("Ambient", SceneLights) of
	true ->
	    St;
	false ->
	    White = {1.0,1.0,1.0,1.0},
	    Lights = [{"Ambient",[{opengl,[{type,ambient},{ambient,White}]}]}],
	    wings_light:import(Lights,St)
    end.

fisheye_vs() ->
<<"
varying vec4 color;

void main(void) 
{
   float near = 0.01;
   float far  = 100.0;
   vec4 pos   = gl_ModelViewMatrix * gl_Vertex;
   pos.w      = 1.0;

   if(length(pos.xyz) > 0.001) {
      color = vec4(0.0,0.0,0.0,1.0);
   } else {  // Discard faces connected to the camera vertex
      color = vec4(1.0,1.0,1.0,1.0);
   }


   float dist = length(pos.xyz);
   pos.xy = pos.xy / dist;

   pos.z = -((far+near)+2.0*pos.z)/(far - near);

   gl_Position = pos;
   // gl_FrontColor = color;
}   
">>.

fisheye_fs() ->
<<"
varying vec4 color;

void main(void) 
{
   // Discard faces connected to the camera vertex
   if(color.x > 0.001)
     discard;
   gl_FragColor = vec4(0.0,0.0,0.0,1.0);
}
">>.

test_img(Name, Bin) ->
    {_,S,M} = now(),
    Str = "_" ++ integer_to_list(S) ++ "_" ++ integer_to_list(M),
    RGB = << <<G:8,G:8,G:8>> || <<G:8>> <= Bin >>,
    Envelope = #e3d_image{image=RGB, width=?SIZE, height=?SIZE},
    wings_image:new_temp(Name ++ Str, Envelope).
