%%
%%  wings_dialog.erl --
%%
%%     This module (re)implements the dialogs using wxWidgets
%%
%%  Copyright (c) 2013 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wings_dialog).
-define(NEED_ESDL, 1). %% Needs to send mouseevents to camera
-include("wings.hrl").
-export([info/3, 
	 ask/3, ask/4, 
	 dialog/3, dialog/4]).

-compile(export_all).

-record(in, {key, type, def, wx, validator, data}).
-record(eh, {dialog,fs,apply,owner}).

%% Display a text window (Info converted to html)
info(Title, Info, Options) ->
    Parent = proplists:get_value(top_frame, Options, get(top_frame)),
    Style  = {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
    Size   = {size, {500, 400}},
    Dialog = wxDialog:new(Parent, ?wxID_ANY, Title,
			  [Style, Size]),
    Panel  = wxHtmlWindow:new(Dialog, []),
    Sizer  = wxBoxSizer:new(?wxVERTICAL),
    Html = text_to_html(Info),
    wxHtmlWindow:appendToPage(Panel, Html),
    wxSizer:add(Sizer, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxWindow:setSizer(Dialog, Sizer),
    wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog),
    keep.


ask_preview(Cmd, Bool, Title, Qs, St) ->
    ask(Bool, Title, preview, Qs, preview_fun(Cmd, St)).

dialog_preview(Cmd, Bool, Title, Qs, St) ->
    dialog(Bool, Title, preview, Qs, preview_fun(Cmd, St)).

%% Currently a modal dialog
%%   (orginal wings let camera events trough)
ask(Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog(true, Title, PreviewCmd, queries(Qs), Fun).
ask(Ask, Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog(Ask, Title, PreviewCmd, queries(Qs), Fun).
ask(Ask, Title, PreviewCmd, Qs, Fun) ->
    dialog(Ask, Title, PreviewCmd, queries(Qs), Fun).

dialog(Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog(true, Title, PreviewCmd, Qs, Fun).
dialog(Ask, Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog(Ask, Title, PreviewCmd, Qs, Fun).
dialog(Ask, Title, PreviewCmd, Qs0, Fun) when is_list(Qs0) ->
    Qs = {vframe_dialog, Qs0, [{buttons, [ok, cancel]}]},
    dialog(Ask, Title, PreviewCmd, Qs, Fun);
dialog(Ask, Title, PreviewCmd, Qs, Fun) when not is_list(Qs) ->
    case element(1,Qs) of
	preview_cmd -> error(Qs);
	drag_preview_cmd -> error(Qs);
	_Assert -> ok
    end,
    {Dialog, Fields} = build_dialog(Ask andalso PreviewCmd, Title, Qs),
    %% io:format("Enter Dialog ~p ~p ~p~n",[Ask,PreviewCmd, Fields]),
    enter_dialog(Ask, PreviewCmd, Dialog, Fields, Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

preview_cmd(Preview = {preview, _}) -> Preview;
preview_cmd(Preview = {drag_preview, _}) -> Preview;
preview_cmd(Qs) -> {no_preview, Qs}.

command_name({Menu,Cmd}, Res) ->
    {Menu,{Cmd,Res}};
command_name({Menu,SubMenu,Cmd}, Res) ->
    {Menu,{SubMenu,{Cmd,Res}}}.

preview_fun(Cmd, St) ->
    fun({dialog_preview,Res}) ->
	    Command = command_name(Cmd, Res),
	    {preview,Command,St};
       (cancel) -> St;
       (Res) ->
	    Command = command_name(Cmd, Res),
	    {commit,Command,St}
    end.

queries(Qs0) ->
    {Labels,Vals} = ask_unzip(Qs0),
    [{hframe,
      [{vframe,Labels},
       {vframe,Vals}]}].

enter_dialog(false, _, _, Fields, Fun) -> % No dialog return def values
    Values = [with_key(Field, Def) ||
		 Field = #in{data=Data, def=Def} <- Fields,
		 Data =/= ignore],
    return_result(Fun, Values, wings_wm:this());
enter_dialog(true, no_preview, Dialog, Fields, Fun) -> %% No preview cmd / modal dialog
    case wxDialog:showModal(Dialog) of
	?wxID_CANCEL -> keep;
	Result ->
	    Values = [get_output(Result, Field) ||
			 Field = #in{data=Data} <- Fields,
			 Data =/= ignore],
	    wxDialog:destroy(Dialog),
	    return_result(Fun, Values, wings_wm:this())
    end;
enter_dialog(true, _Preview, Dialog, Fields, Fun) ->
    Env = wx:get_env(),
    spawn_link(fun() ->
		       wx:set_env(Env),
		       Me = self(),
		       Forward = fun(Event, _) ->
					 %% io:format("Send event ~p to wings~n", [Event]),
					 wxDialog:show(Dialog, [{show,false}]),
					 wings_wm:psend(dialog_blanket, Event),
					 Me ! closed
				 end,

		       wxDialog:connect(Dialog, command_button_clicked,
					[{id, ?wxID_OK},
					 {lastId, ?wxID_NO},
					 {callback,Forward}]),
		       wxDialog:show(Dialog),
		       receive closed -> ok end
	       end),
    State = #eh{dialog=Dialog, fs=Fields, apply=Fun, owner=wings_wm:this()},
    Op = {push,fun(Ev) -> event_handler(Ev, State) end},
    {TopW,TopH} = wings_wm:top_size(),
    wings_wm:new(dialog_blanket, {0,0,highest}, {TopW,TopH}, Op),
    keep.

notify_event_handler(false, _Msg) -> fun() -> ignore end;
notify_event_handler(no_preview, _) -> fun() -> ignore end;
notify_event_handler(_, Msg) -> fun() -> wings_wm:psend(dialog_blanket, Msg) end.

event_handler(#wx{id=?wxID_CANCEL}, #eh{dialog=Dialog, apply=Fun, owner=Owner}) ->
    wxDialog:destroy(Dialog),
    #st{}=St = Fun(cancel),
    wings_wm:send(Owner, {update_state,St}),
    delete;
event_handler(#wx{id=Result}=_Ev, #eh{dialog=Dialog, fs=Fields, apply=Fun, owner=Owner}) ->
    %% io:format("Ev closing ~p~n",[_Ev]),
    Values = [get_output(Result, Field) ||
		 Field = #in{data=Data} <- Fields,
		 Data =/= ignore],
    wxDialog:destroy(Dialog),
    return_result(Fun, Values, Owner),
    delete;
event_handler(preview, #eh{fs=Fields, apply=Fun, owner=Owner}) ->
    Values = [get_output(preview, Field) ||
		 Field = #in{data=Data} <- Fields,
		 Data =/= ignore],
    case Fun({dialog_preview,Values}) of
	{preview,#st{}=St0,#st{}=St} ->
	    wings_wm:send_after_redraw(Owner, {update_state,St}),
	    wings_wm:send(Owner, {current_state,St0});
	{preview,Action,#st{}=St}->
	    wings_wm:send_after_redraw(Owner, {action,Action}),
	    wings_wm:send(Owner, {current_state,St});
	Action when is_tuple(Action); is_atom(Action) ->
	    io:format("~p:~p: ~p~n",[?MODULE,?LINE,{preview,[Owner,{action,Action}]}]),
	    wings_wm:send(Owner, {action,Action})
    end;
event_handler(#mousebutton{x=X0,y=Y0}=Ev, _) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Win = wings_wm:geom_below(X, Y),
    wings_wm:send(Win, {camera,Ev,keep});
event_handler(#mousemotion{}, _) -> keep;
event_handler(_Ev, _) ->
    %% io:format("unhandled Ev ~p~n",[_Ev]),
    keep.


get_output(Result, In) ->
    get_output1(Result, In).

get_output1(_, In=#in{type=checkbox, wx=Ctrl}) ->
    with_key(In,wxCheckBox:getValue(Ctrl));
get_output1(_, In=#in{type=radiobox, wx=Ctrl, data=Keys}) ->
    ZeroIndex = wxRadioBox:getSelection(Ctrl),
    with_key(In, lists:nth(ZeroIndex+1, Keys));
get_output1(_, In=#in{type=filepicker, wx=Ctrl}) ->
    with_key(In,wxFilePickerCtrl:getPath(Ctrl));
get_output1(_, In=#in{type=slider, wx=Ctrl, data={Convert,_}}) ->
    with_key(In,Convert(wxSlider:getValue(Ctrl)));
get_output1(_, In=#in{type=choice, wx=Ctrl}) ->
    with_key(In,wxChoice:getClientData(Ctrl,wxChoice:getSelection(Ctrl)));
get_output1(_, In=#in{type=text, def=Def, wx=Ctrl, validator=Validate}) ->
    Str = wxTextCtrl:getValue(Ctrl),
    Res = if is_integer(Def) ->
		  wings_ask:eval_integer(validate(Validate, Str));
	     is_float(Def)  ->
		  wings_ask:eval_float(validate(Validate, Str));
	     is_list(Def) ->
		  Str
	  end,
    with_key(In, Res);
get_output1(Result, In=#in{type=dialog_buttons}) ->
    Atom = case Result of
	       ?wxID_OK -> ok;
	       ?wxID_CANCEL -> cancel;
	       ?wxID_YES -> yes;
	       ?wxID_NO -> no
	   end,
    with_key(In, Atom).

with_key(#in{key=undefined}, Value) -> Value;
with_key(#in{key=Key}, Value) ->  {Key, Value}.

validate(Fun, Input) ->
    case Fun(Input) of
	ok -> Input;
	Str when is_list(Str) -> Str
    end.

return_result(Fun, Values, Owner) ->
    case Fun(Values) of
	ignore ->
	    ok;
	#st{}=St ->
	    wings_wm:send(Owner, {new_state,St});
	{commit,#st{}=St0,#st{}=St}->
	    wings_wm:send(Owner, {current_state,St0}),
	    wings_wm:send_after_redraw(Owner, {new_state,St});
	{commit,Action,#st{}=St}->
	    wings_wm:send(Owner, {current_state,St}),
	    wings_wm:send_after_redraw(Owner, {action,Action});
	Action when is_tuple(Action); is_atom(Action) ->
	    wings_wm:send(Owner, {action,Action})
    end,
    keep.

ask_unzip(Qs) ->
    ask_unzip(Qs, [], []).
ask_unzip([{Label,{menu,_,_}=Menu}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [Menu|AccB]);
ask_unzip([{Label,{menu,_,_,_}=Menu}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [Menu|AccB]);
ask_unzip([{Label,Def}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def}|AccB]);
ask_unzip([{Label,Def,Flags}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def,Flags}|AccB]);
ask_unzip([], Labels, Vals) ->
    {lists:reverse(Labels),lists:reverse(Vals)}.

build_dialog(false, _Title, Qs) ->
    Fs = build(false, Qs, undefined, undefined, []),
    {undefined, lists:reverse(Fs)};
build_dialog(AskType, Title, Qs) ->
    Parent = get(top_frame),
    Style  = {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
    Dialog = wxDialog:new(Parent, ?wxID_ANY, Title, [Style]),
    Panel  = wxPanel:new(Dialog, []),
    Top    = wxBoxSizer:new(?wxVERTICAL),
    Sizer  = wxBoxSizer:new(?wxVERTICAL),
    Fields0 = build(AskType, Qs, Panel, Sizer, []),
    wxWindow:setSizer(Panel, Sizer),
    wxSizer:add(Top, Panel, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    Fields = setup_buttons(Dialog, Top, Fields0),
    wxWindow:setSizerAndFit(Dialog, Top),
    {Dialog, lists:reverse(Fields)}.

setup_buttons(Dialog, Top, [DB=#in{type=dialog_buttons, wx=Fun}|In]) ->
    Object = Fun(Dialog, Top),
    [DB#in{wx=Object}|In];
setup_buttons(_, _, Fields) ->
    Fields.

build(Ask, {vframe_dialog, Qs, Flags}, Parent, Sizer, []) ->
    Def = proplists:get_value(value, Flags, ?wxID_OK),
    Buttons = proplists:get_value(buttons, Flags, [ok, cancel]),
    ButtMask = lists:foldl(fun(ok, Butts)     -> ?wxOK bor Butts;
			      (cancel, Butts) -> ?wxCANCEL bor Butts;
			      (yes, Butts)    -> ?wxYES bor Butts;
			      (no,  Butts)    -> ?wxNO bor Butts
			   end, 0, Buttons),
    Create = fun(Dialog, TopSizer) ->
		     Ok = wxDialog:createButtonSizer(Dialog, ButtMask),
		     wxSizer:add(TopSizer, Ok, [{proportion, 0},
						{flag, ?wxEXPAND bor ?wxALL},
						{border, 5}]),
		     case Ask of
			 no_preview -> %% I.e. non preview
			     Close = fun(#wx{id=Id},_) ->
					     wxDialog:endModal(Dialog, Id)
				     end,
			     wxDialog:connect(Dialog, command_button_clicked,
					      [{id, ?wxID_NO}, {callback,Close}]);
			 _ ->
			     ignore %% Preview connects it self
		     end,
		     Ok
	     end,
    In = build(Ask, {vframe, Qs}, Parent, Sizer, []),
    [#in{key=proplists:get_value(key,Flags), def=Def, data=proplists:get_value(key,Flags,ignore),
	 type=dialog_buttons, wx=Create}|In];

build(Ask, {vframe, Qs}, Parent, Sizer, In) ->
    build(Ask, {vframe, Qs, []}, Parent, Sizer, In);
build(Ask, {vframe, Qs, Flags}, Parent, Sizer, In) ->
    build_box(Ask, ?wxVERTICAL, Qs, Flags, Parent, Sizer, In);
build(Ask, {hframe, Qs}, Parent, Sizer, In) ->
    build(Ask, {hframe, Qs, []}, Parent, Sizer, In);
build(Ask, {hframe, Qs, Flags}, Parent, Sizer, In) ->
    build_box(Ask, ?wxHORIZONTAL, Qs, Flags, Parent, Sizer, In);

build(Ask, {vradio, Alternatives, Def}, Parent, Sizer, In) ->
    build(Ask, {vradio, "", Alternatives, Def}, Parent, Sizer, In);
build(Ask, {vradio, Name, Alternatives, Def}, Parent, Sizer, In) ->
    build_radio(Ask, Name, Def, ?wxRA_SPECIFY_COLS, Alternatives, Parent, Sizer, In);
build(Ask, {hradio, Alternatives, Def}, Parent, Sizer, In) ->
    build(Ask, {hradio, "", Alternatives, Def}, Parent, Sizer, In);
build(Ask, {hradio, Name, Alternatives, Def}, Parent, Sizer, In) ->
    build_radio(Ask, Name, Def, ?wxRA_SPECIFY_ROWS, Alternatives, Parent, Sizer, In);

build(Ask, {label, Label}, Parent, Sizer, In) ->
    build(Ask, {label, Label, []}, Parent, Sizer, In);
build(Ask, {label, Label, Flags}, Parent, Sizer, In)
  when Ask =/= false ->
    Limit = proplists:get_value(break, Flags, infinite),
    {_,Lines0=[First|_]} = wings_text:break_lines([Label], Limit),
    Lines = lists:foldr(fun(Row, Acc) when Row =:= First -> [Row|Acc];
			   (Row, Acc) -> ["\n", Row|Acc]
			end, [], Lines0),
    Text = wxStaticText:new(Parent, ?wxID_ANY, Lines),
    add_sizer(label, Sizer, Text),
    In;
build(Ask, panel, _Parent, Sizer, In)
  when Ask =/= false ->
    wxSizer:addSpacer(Sizer, 20),
    In;
build(Ask, separator, Parent, Sizer, In)
  when Ask =/= false ->
    Separator = wxStaticLine:new(Parent),
    add_sizer(separator, Sizer, Separator),
    In;

build(Ask, {text, Def}, Parent, Sizer, In) ->
    build(Ask, {text, Def, []}, Parent, Sizer, In);
build(Ask, {text, Def, Flags}, Parent, Sizer, In) ->
    {_Max0,Validator,_Charset} = wings_ask:validator(Def, Flags),
    Create = fun() ->
		     Ctrl = wxTextCtrl:new(Parent, ?wxID_ANY, [{value, to_str(Def)}]),
		     add_sizer(text, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 type=text, wx=create(Ask,Create), validator=Validator}|In];

build(Ask, {slider, {text, Def, Flags}}, Parent, Sizer, In) ->
    {_Max0,Validator,_Charset} = wings_ask:validator(Def, Flags),
    Create = fun() -> create_slider(Ask, Def, Flags, Validator, Parent, Sizer) end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 type=text, wx=create(Ask,Create), validator=Validator}|In];

build(Ask, {slider, Flags}, Parent, Sizer, In) ->
    Def = proplists:get_value(value, Flags),
    Range = proplists:get_value(range, Flags),
    false = undefined == Def,
    false = undefined == Range,
    {Min, Value, Max, Style, ToText, ToSlider} = slider_style(Def, Range),
    Create = fun() ->
		     Ctrl = wxSlider:new(Parent, ?wxID_ANY, Value, Min, Max, [{style, Style}]),
		     add_sizer(slider, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def, data={ToText, ToSlider},
	 type=slider, wx=create(Ask,Create)}|In];

build(Ask, {button, {text, Def, Flags}}, Parent, Sizer, In) ->
    Create = fun() ->
		     Props = proplists:get_value(props, Flags, []),
		     What = case proplists:get_value(dialog_type, Props, open_dialog) of
				open_dialog -> ?wxFLP_OPEN;
				save_dialog -> ?wxFLP_SAVE
			    end,
		     Filter = wings_file:file_filters(Props),
		     Ctrl = wxFilePickerCtrl:new(Parent, ?wxID_ANY, 
						 [{style, What bor ?wxFLP_USE_TEXTCTRL},
						  {path, Def}, 
						  {wildcard, Filter}]), 
		     add_sizer(button, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 type=filepicker, wx=create(Ask,Create)}|In];

build(Ask, {menu, Entries, Def, Flags}, Parent, Sizer, In) ->
    Create =
	fun() ->
		Ctrl = wxChoice:new(Parent, ?wxID_ANY),
		lists:foldl(fun({Str, Tag}, N) ->
				    wxChoice:append(Ctrl, Str, Tag),
				    Def =:= Tag andalso wxChoice:setSelection(Ctrl, N),
				    N + 1
			    end, 0, Entries),
		add_sizer(choice, Sizer, Ctrl),
		Ctrl
	end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 type=choice, wx=create(Ask,Create)}|In];

build(Ask, {table, [Header|Rows], Flags}, Parent, Sizer, In) ->
    Create = 
	fun() ->
		Options = [{style, ?wxLC_REPORT}, 
			   {size, {min(tuple_size(Header)*80, 500), 
				   min((2+length(Rows))*25, 800)}}],
		Ctrl = wxListCtrl:new(Parent, Options),
		AddHeader = fun(HeadStr, Column) ->
				    wxListCtrl:insertColumn(Ctrl, Column, HeadStr, []),
				    Column + 1
			    end,
		lists:foldl(AddHeader, 0, tuple_to_list(Header)),
		case proplists:get_value(col_widths, Flags) of
		    undefined -> ok;
		    Widths ->
			SetWidth = fun(Width, Column) ->
					   wxListCtrl:setColumnWidth(Ctrl, Column, Width*8),
					   Column + 1
				   end,
			lists:foldl(SetWidth, 0, tuple_to_list(Widths))
		end,
		Add = fun({_, Str}, {Row, Column}) ->
			      wxListCtrl:setItem(Ctrl, Row, Column, Str),
			      {Row, Column+1}
		      end,
		lists:foldl(fun(Row, N) ->
				    wxListCtrl:insertItem(Ctrl, N, ""),
				    lists:foldl(Add, {N, 0}, tuple_to_list(Row)),
				    N + 1
			    end, 0, Rows),
		add_sizer(table, Sizer, Ctrl),
		Ctrl
	end,
    create(Ask,Create),
    %% [#in{key=proplists:get_value(key,Flags), def=Rows,
    %% 	 type=table, wx=create(Ask,Create)}|In];
    In;

build(Ask, {Label, Def}, Parent, Sizer, In) ->
    build(Ask, {Label, Def, []}, Parent, Sizer, In);
build(Ask, {Label, Def, Flags}, Parent, Sizer, In) 
  when is_boolean(Def) ->
    Create = fun() ->
		     Ctrl = wxCheckBox:new(Parent, ?wxID_ANY, Label),
		     wxCheckBox:setValue(Ctrl, Def),
		     add_sizer(checkbox, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), 
	 def=Def, type=checkbox, wx=create(Ask, Create)}|In];

build(Ask, {help, Title, Fun}, Parent, Sizer, In) ->
    TopFrame = get(top_frame),
    Display = fun(_,_) ->
		      info(Title, Fun(), [{top_frame, TopFrame}])
	      end,
    Create = fun() ->
		     Button = wxButton:new(Parent, ?wxID_HELP),
		     wxButton:connect(Button, command_button_clicked, [{callback, Display}]),
		     add_sizer(button, Sizer, Button),
		     Button
	     end,
    create(Ask,Create),
    In;

build(false, _Q, _Parent, _Sizer, In) -> 
    In;
build(Ask, Q, _Parent, _Sizer, In) ->
    io:format("~p:~p: Unhandled ask=~p, ~p~n  From: ~p",
	      [?MODULE,?LINE,Ask,Q, erlang:process_info(self(), current_stacktrace)]),
    In.

build_box(false, _Type, Qs, _, Parent, _Top, In0) ->
    lists:foldl(fun(Q, Input) ->
			build(false, Q, Parent, undefined, Input)
		end, In0, Qs);
build_box(Ask, Type, Qs, Flags, Parent, Top, In0) ->
    Sizer = case proplists:get_value(title, Flags) of
		undefined -> wxBoxSizer:new(Type);
		Title -> wxStaticBoxSizer:new(Type, Parent, [{label, Title}])
	    end,
    Input = lists:foldl(fun(Q, Input) ->
				build(Ask, Q, Parent, Sizer, Input)
			end, In0, Qs),
    add_sizer({box, Type}, Top, Sizer),
    Input.

build_radio(Ask, Name, Def, Direction, Alternatives, Parent, Sizer, In) ->
    {Strs,Keys} = lists:unzip(Alternatives),
    true = lists:member(Def, Keys),
    Create = fun() ->
		     Ctrl = wxRadioBox:new(Parent, 1, Name,
					   ?wxDefaultPosition, ?wxDefaultSize,
					   Strs, [{majorDim, 1}, {style, Direction}]),
		     add_sizer({radiobox, Direction}, Sizer, Ctrl),
		     wxRadioBox:enable(Ctrl, pos(Def, Keys)),
		     Ctrl
	     end,
    [#in{def=Def, type=radiobox, wx=create(Ask, Create), data=Keys}|In].

create_slider(Ask, Def, Flags, Validator, Parent, TopSizer) when is_number(Def) ->
    Range = proplists:get_value(range, Flags),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    {Min, Value, Max, Style, ToText, ToSlider} = slider_style(Def, Range),
    Slider = wxSlider:new(Parent, ?wxID_ANY, Value, Min, Max, [{style, Style}]),
    Text = wxTextCtrl:new(Parent, ?wxID_ANY, [{value, to_str(Def)}]),
    wxSizer:add(Sizer, Slider, [{proportion,3}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, Text,   [{proportion,1}]),
    add_sizer(slider, TopSizer, Sizer),
    PreviewFun = notify_event_handler(Ask, preview),
    UpdateText = fun(#wx{event=#wxCommand{commandInt=Where}}, _) ->
			 PreviewFun(),
			 wxTextCtrl:setValue(Text, to_str(ToText(Where)))
		 end,
    wxSlider:connect(Slider, command_slider_updated, [{callback, UpdateText}]),
    UpdateSlider = fun(#wx{event=#wxCommand{cmdString=Str}}, _) ->
			   case Validator(Str) of
			       ok -> 
				   PreviewFun(),
				   wxSlider:setValue(Slider, ToSlider(Str));
			       _ -> ignore
			   end
		   end,
    wxTextCtrl:connect(Text, command_text_updated, [{callback, UpdateSlider}]),
    Text.

slider_style(Def, {Min, Max})
  when is_integer(Def), Def >= Min, Def =< Max, Min < Max ->
    ToText = fun(Value) -> Value end,
    ToSlider = fun(Str) -> wings_ask:eval_integer(Str) end,
    {Min, Def, Max, ?wxSL_HORIZONTAL bor ?wxSL_LABELS, ToText, ToSlider};
slider_style(Def, {Min, Max})
  when is_float(Def), Def >= Min, Def =< Max, Min < Max ->
    ToSlider = fun(ValueStr) ->
		       Value = wings_ask:eval_float(ValueStr),
		       Step = (Max - Min) / 100,
		       round((Value - Min) / Step)
	       end,
    ToText = fun(Percent) ->
		     Step = (Max - Min) / 100,
		     Min + Percent * Step
	     end,
    {0, ToSlider(to_str(Def)), 100, ?wxSL_HORIZONTAL, ToText, ToSlider}.

add_sizer(What, Sizer, Ctrl) ->
    {Propportion, Border, Flags} = sizer_flags(What, wxBoxSizer:getOrientation(Sizer)),
    wxSizer:add(Sizer, Ctrl, [{proportion, Propportion}, {border, Border}, {flag, Flags}]).

sizer_flags(label, ?wxHORIZONTAL)     -> {0, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(label, ?wxVERTICAL)       -> {1, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(separator, ?wxHORIZONTAL) -> {1, 5, ?wxALL bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(separator, ?wxVERTICAL)   -> {0, 5, ?wxALL bor ?wxEXPAND};
sizer_flags(text, ?wxHORIZONTAL)      -> {1, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(slider, ?wxHORIZONTAL)    -> {3, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(button, ?wxHORIZONTAL)    -> {0, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(choice, ?wxHORIZONTAL)    -> {0, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(checkbox, ?wxHORIZONTAL)  -> {1, 0 ,?wxALIGN_CENTER_VERTICAL};
sizer_flags(table,  _)                -> {4, 0, ?wxEXPAND};
sizer_flags({radiobox, Dir}, Dir)     -> {5, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags({radiobox, _}, _)         -> {0, 0, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags({box, Dir}, Dir)          -> {1, 0, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags({box, _}, _)              -> {0, 0, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(_, ?wxHORIZONTAL)         -> {1, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(_, ?wxVERTICAL)           -> {0, 0, ?wxEXPAND}.

create(false, _) -> undefined;
create(_, Fun) -> Fun().

to_str(Number) when is_integer(Number) ->
    integer_to_list(Number);
to_str(Float) when is_float(Float) ->
    wings_util:nice_float(Float);
to_str(List = [C|_]) when is_integer(C) ->
    List.

pos(C, S) -> pos(C, S, 0).
pos(C, [C|_Cs], I) -> I;
pos(C, [_|Cs], I) -> pos(C, Cs, I+1);
pos(_, [], _I) -> 0.

text_to_html(Paragraphs) ->
    Header = ["<html>"],
    Html = text_to_html(Paragraphs, Header),
    lists:reverse(["</html>"|Html]).

text_to_html([[_|_] = Paragraph|Ps], Acc) ->
    text_to_html(Ps, ["</p>", paragraph_to_html(Paragraph), "<p>"|Acc]);
text_to_html([Table = {table,_,_,_}|Ps], Acc) ->
    text_to_html(Ps, [table_to_html(Table)|Acc]);
text_to_html([{bullet, List}|Rest], Acc) ->
    BulletList = ["</p><ul>", 
		  ["<li>" ++ paragraph_to_html(Item) ++ "</li>" || Item <- List],
		  "</ul><p>"],
    text_to_html(Rest, [BulletList|Acc]);
text_to_html([], Acc) -> Acc.

paragraph_to_html([C|Text]) when is_integer(C) ->
    [C|paragraph_to_html(Text)];
paragraph_to_html([{bold, Text}|Rest]) ->
    ["<b>", paragraph_to_html(Text), "</b>"| paragraph_to_html(Rest)];
paragraph_to_html([{ul, Text}|Rest]) ->
    ["<u>", paragraph_to_html(Text), "</u>"| paragraph_to_html(Rest)];
paragraph_to_html([Table={table, _, _, _}|Rest]) ->
    ["</p>", table_to_html(Table), "<p>" | paragraph_to_html(Rest)];
paragraph_to_html([C|Text]) when is_list(C) ->
    [paragraph_to_html(C), paragraph_to_html(Text)];
paragraph_to_html([]) -> [].

table_to_html({table, _, Header, Items}) ->
    ["<p><b>", Header, "</b></p><table>", 
     [table_row_to_html(Row) || Row <- Items],
     "</table>"].

table_row_to_html(Row) when is_list(Row) ->
    ["<tr>", ["<td>" ++ paragraph_to_html(Column) ++ "</td>" || Column <- Row], "</tr>"].
    
