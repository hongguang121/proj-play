/**
 * Created by Administrator on 2017/3/2 0002.
 */

//查找商品，新增或删除文章
var projectUrl = function() {
    // 获取当前网址，如： http://localhost:8083/uimcardprj/share/meun.jsp
    var curWwwPath = window.document.location.href;
    // 获取主机地址之后的目录，如： uimcardprj/share/meun.jsp
    var pathName = window.document.location.pathname;
    var pos = curWwwPath.indexOf(pathName);
    // 获取主机地址，如： http://localhost:8083
    var localhostPath = curWwwPath.substring(0, pos);
    // 获取带"/"的项目名，如：/uimcardprj
    var projectName = pathName
        .substring(0, pathName.substr(1).indexOf('/') + 1);
    return localhostPath;
};

var date = new Date();
var receive = "";
var preferredPic = ""; //封面图片
var evt_lst = []; //添加内容顺序表
var remb_text = {text: {title: '', author: '', summary: '', contents: []}}; //文本缓存 {text_id: 'text_0', content: 'test0'}, {text_id: 'text_1', content: 'test1'}
var remb_pic = {images: []}; //图片缓存
var remb_good = []; //商品缓存
var add = 1; //限制‘添加新的图片’按钮点击次数 0:unlock 1:lock
var hobby = '{"id_1": "美妆", "id_2": "汽车", "id_3": "时尚", "id_4": "旅游", "id_5": "游戏", "id_6": "动漫", "id_7": "音乐", "id_8": "美食", "id_9": "艺术", "id_10": "摄影", "id_11": "读书", "id_12": "社交", "id_13": "运动", "id_14": "影视", "id_15": "极客"}';
var json_hobby = JSON.parse(hobby);
var send_hob = "";
var login_key = "";
var req_submit_url = projectUrl() + "/advert/poem/new.html";
var req_upload_url = projectUrl() + "/common/resource/image/upload.json";
var good_pic_url = "https://mall.mouchina.com/shoppingmall/image/";

//模拟文章列表
var art = {article: [{title: "外祖母的扑克牌", title_img: "/h5/img/pic/background.jpg", summary: "曾疑惑为什么外祖母在离世前的那一段日子...", text: ""},
                     {title: "圣路易斯", title_img: "/h5/img/pic/1_standard.jpg", summary: "那些动物园的解体与其说是由于经营不善...", text: ""},
                     {title: "一朵云的影像计划！从 DIY 到手机摄影攻略 | 「摄影养成邪会」第八回", title_img: "/h5/img/pic/p2233958916.jpg", summary: "恰到好处的斑驳——风的层次...", text: ""},
                     {title: "滨江小城", title_img: "/h5/img/pic/p2240269175.jpg", summary: "生于斯，长于斯，稍稍离远些方才明白...", text: ""}]};

var cnt_pic = 0; //用来生成图片id
var curr_pic_cnt = 0; //限制图片上传数量，不能超过5

$(document).ready(function() {

    //屏幕大小适应
    var curr_screen_width = $(window).width();
    var main_view_width = 1280;
    //var left_shifting = (curr_screen_width - main_view_width) / 2;

    $(window).scroll(function() {
        if ($(document).scrollTop()<=0){
            //alert("滚动条已经到达顶部为0");
        }

        if ($(document).scrollTop() >= $(document).height() - $(window).height()) {
            //alert("滚动条已经到达底部为" + $(document).scrollTop());
        }
    });

    //距屏幕左侧距离
    var left_shifting = 0;
    var left_change = function(){
        if (curr_screen_width > 1280){
            $(".banner").css("width", "100%");
            $(".navbar").css("width", "100%");
            $(".footer").css("width", "100%");
            $(".user").css({"width": "1280px", "margin": "0 auto"});
        } else {
            $(".banner").css("width", "1280px");
            $(".navbar").css("width", "1280px");
            $(".footer").css("width", "1280px");
            $(".user").css("width", "1280px");
            $(".left-panel").css("left", (left_shifting + 1) + "px");
        }
    };

    left_change();

    $(window).resize(function(){
        curr_screen_width = $(window).width();
        //left_shifting = (curr_screen_width - main_view_width) / 2;
        $(".banner").css("width", "1280px");
        $(".navbar").css("width", "1280px");
        $(".footer").css("width", "1280px");
        left_change();

        if (curr_screen_width > main_view_width){
            $(".banner").css("width", "100%");
            $(".navbar").css("width", "100%");
            $(".footer").css("width", "100%");
        }
        if (curr_screen_width < main_view_width){
            $(".user").css({"width": "1280px", "margin": "0 auto"});
            $(".left-panel").css("left", (left_shifting + 1) + "px");
        }
    });

    var top = function(){
        $(".to-top").hide();
        var set_height = setInterval(function(){
            var body_height = $("body").height();
            var body_width = $("body").width();
            if ($(window).scrollTop() > 500){
                if ($(window).width() < 1000){
                    $(".to-top").css("left", body_width - 150);
                    $(".to-top").animate({top: body_height - 200}, 500).show("slow");
                } else {
                    $(".to-top").css("left", body_width - 150);
                    $(".to-top").animate({top: $(window).scrollTop() + 840}, 500).show("slow");
                }
            } else {
                $(".to-top").hide();
            }
        }, 1500);
    };

    var change_high = function(){
        var mg = $(".middle-panel").height();
        //var lg = $(".left-panel").height();
        //var rg = $(".right-panel").height();
        //console.log(hg);
        $(".left-panel").css("height", mg + 2);
        $(".right-panel").css("height", mg + 2);
        //$(".middle-panel").height();
    };

    change_high();

    /*var left_click = 0;

    $(".hide-show-left").click(function(){
        if (left_click == 0) {
            $(".left-panel").hide();
            $(".hide-show-left").html("<img src='/h5/img/right.svg' width='40' style='position: relative; top: 15px; left: -7px;'/>");

            if (right_click == 1){
                $(".middle-panel").css("width", "1280px");
            } else {
                $(".middle-panel").css("width", "1024px");
            }
            left_click = 1;
        } else {
            $(".left-panel").show();
            $(".hide-show-left").html("<img src='/h5/img/left.svg' width='40' style='position: relative; top: 15px; left: -7px;'/>");

            if (right_click == 1){
                $(".middle-panel").css("width", "1024px");
            } else {
                $(".middle-panel").css("width", "768px");
            }
            left_click = 0;
        }
        change_high();
    });

    var right_click = 0;
    $(".hide-show-right").click(function(){
        if (right_click == 0) {
            $(".right-panel").hide();
            $(".hide-show-right").html("<img src='/h5/img/left.svg' width='40' style='position: relative; top: 15px; left: -7px;'/>");

            if (left_click == 1){
                $(".middle-panel").css("width", "1280px");
            } else {
                $(".middle-panel").css("width", "1024px");
            }

            right_click = 1;
        } else {
            $(".right-panel").show();
            $(".hide-show-right").html("<img src='/h5/img/right.svg' width='40' style='position: relative; top: 15px; left: -7px;'/>");

            if (left_click == 1){
                $(".middle-panel").css("width", "1024px");
            } else {
                $(".middle-panel").css("width", "768px");
            }
            right_click = 0;
        }
        change_high();
    });
*/
    var rolling = function(){
        if ($(window).scrollTop() > $(".left-panel").offset().top){
            //alert("over")
        }
    };

    rolling();

    $("#title").keydown(function(evt){
        switch (evt.keyCode){
            case 13: $("#auth").select();
        }
    });

    var art_list = {
        "position": "relative",
        "width": "85%",
        "height": "240px",
        "background-color": "white",
        "border": "solid 1px #e6e6e6",
        "transition": "box-shadow 1s",
        "overflow": "hidden"
    };

    var art_pic = {
        "position": "relative",
        "width": "100%",
        "height": "100px",
        "margin-top": "8px",
        "float": "left",
        "overflow": "hidden"
    };

    var art_title = {
        "position": "relative",
        "width": "90%",
        "height": "30px",
        "margin-left": "5px",
        "margin-right": "5px",
        "margin-bottom": "5px",
        "float": "left",
        "overflow": "hidden",
        "font-size": "15px",
        "font-weight": "800",
        "text-align": "center"
    };

    var art_summary = {
        "position": "relative",
        "width": "90%",
        "height": "50px",
        "margin-left": "10px",
        "margin-right": "10px",
        "padding-bottom": "5px",
        "line-height": "20px",
        "float": "left",
        "overflow": "hidden",
        "font-size": "12px",
        "z-index": "0"
    };

    var art_edit = {
        "position": "relative",
        "width": "50%",
        "height": "45px",
        "border-top": "solid 1px #eee",
        "float": "left",
        "background-color": "white",
        "z-index": "5"
    };

    var art_del = {
        "position": "relative",
        "width": "50%",
        "height": "45px",
        "border-top": "solid 1px #eee",
        "float": "left",
        "background-color": "white",
        "z-index": "5"
    };

    //show_article

    var index = 0; //开始页序号
    var page = 3; //每页条目数
    var page_sum; //总页数

    var pre_article = function(){
        $.ajax({
            //url: projectUrl() + "/advert/poem/list.html",
            //url: "http://adminmou1.mouchina.com/advert/poem/list.json",
            url: "http://localhost:8100/upload",
            dataType: 'json',
            success: function(data){
                page_sum = Math.ceil(data.data.length / page); //页数
                art = data;
                console.log(art);
                show_article(index, page);
            },
            error: function(xhr){
                console.log(xhr);
            }
        })
    };

    pre_article();

    var show_article = function(id, page){ //id:起始下标，page:页面大小
        //for (var i = id, cnt = 0; i < art.article.length; i++, cnt++){ //example
        for (var i = id, cnt = 0; i < art.data.length; i++, cnt++){
            if (cnt < page){
                var art_list_id = "article_list_id_" + i;
                var art_pic_id = "article_pic_id_" + i;
                var art_title_id = "article_title_id_" + i;
                var art_summary_id = "article_summary_id_" + i;
                var art_edit_id = "article_edit_id_" + art.data[i].id;
                var art_del_id = "article_del_id_" + art.data[i].id;

                $("#art-list").append(
                    "<div class='list-group' id=" + art_list_id + ">" +
                    //"<div class=" + art_pic_id + "><img src=" + art.article[i].title_img + " width='200' /></div>" +
                    "<div class=" + art_pic_id + "><img src=" + art.data[i].preferredPic + " width='200' /></div>" +
                    //"<div class=" + art_title_id + ">" + art.article[i].title + "</div>" +
                    "<div class=" + art_title_id + ">" + art.data[i].title + "</div>" +
                    //"<div class=" + art_summary_id + ">" + art.article[i].summary + "</div>" +
                    //"<div class=" + art_summary_id + ">" + art.data[i].contents[1].summary.substring(0, 12) + "..." + "</div>" +
                    "<div class=" + art_edit_id + " id=" + art_edit_id + "><img src='h5/img/wengaobianji.svg' class=" + art_edit_id + " width='20' /></div>" +
                    //"<div class=" + art_edit_id + "><img src='/h5/img/wengaobianji.svg' width='20' /></div>" +
                    "<div class=" + art_del_id + " id=" + art_del_id + "><img src='h5/img/删除-3.svg' class=" + art_del_id + " width='20' /></div></div>"
                    //"<div class=" + art_del_id + "><img src='/h5/img/删除-3.svg' width='20' /></div></div>"
                );

                $("#" + art_list_id).css(art_list)
                .hover(function(){
                    $(this).css({"box-shadow": "0px 0px 10px 5px #e8e8e8",
                                 "cursor": "pointer"});
                }, function(){
                    $(this).css("box-shadow", "");
                }).click(function(){

                });

                $("." + art_pic_id).css(art_pic);
                $("." + art_title_id).css(art_title);
                $("." + art_summary_id).css(art_summary);
                $("#" + art_edit_id).css(art_edit).hover(function(){
                    $(this).css("background-color", "#eee");
                }, function(){
                    $(this).css("background-color", "");
                });
                $("#" + art_del_id).css(art_del).hover(function(){
                    $(this).css("background-color", "#eee");
                }, function(){
                    $(this).css("background-color", "");
                });

                index = i + 1; //下一页的起始下标
            }
        }
    };

    $("#article").click(function (e) {
        var id = $(e.target).attr("class");
        console.log(id);
        if (id.substring(8, 12) == "edit"){
            //edit ajax
            var eid = id.substring(16);
            edit(eid);
            //alert(eid);
        } else if (id.substring(8, 11) == "del"){
            //del ajax
            var did = id.substring(15);
            del(did);
            //alert(did);
        }
    });

    //判断item是否是arr的成员
    var member = function(arr, item){
        for (var i = 0; i < arr.length; i++){
            if (arr[i] == item){
                return true;
            }
        }
        return false;
    };

    var del = function(did) {
        $.ajax({
            url: projectUrl() + "/advert/poem/delete.json?poemAdvertId=" + did,
            success: function(data){
            },
            error: function(xhr){
                console.log(xhr);
            }
        })
    };

    $("#art-left").click(function(){
        if (index < art.data.length) {
            $("#art-list").remove();
            $("#article").append("<div id='art-list'></div>");
            show_article(index, page);
        } else {
            $("#art-list").remove();
            $("#article").append("<div id='art-list'></div>");
            show_article(0, page);
        }
    });

    $("#art-right").click(function(){
        if (index < art.data.length) {
            $("#art-list").remove();
            $("#article").append("<div id='art-list'></div>");
            show_article(index, page);
        } else {
            $("#art-list").remove();
            $("#article").append("<div id='art-list'></div>");
            show_article(0, page);
        }
    });

    //show_hobby
    var show_hobby = function(){
        var init = {
            "position": "relative",
            "width": "100px",
            "height": "30px",
            "margin-left": "5px",
            "margin-right": "5px",
            "margin-top": "10px",
            "margin-bottom": "10px",
            "border": "solid 1px #EEEEEE",
            "border-radius": "5px",
            "box-shadow": "0px 0px 5px 0px #EEEEEE",
            "float": "left",
            "padding-top": "5px",
            "text-align": "center",
            "cursor": "pointer"
        };

        for (var key in json_hobby){
            var hid = key;
            $(".hobby-div").append("<div class='hobby' id=" + hid + ">" + json_hobby[hid] + "</div>");
            $("#" + hid).css(init);
            change_high();
        }
    };

    show_hobby();

    var hob = "";
    $(".hobby").click(function(){
        var hid = $(this).attr("id");
        if (hid == hob){
            $("#" + hid).css("background", "white");
            hob = "";
        } else {
            $("#" + hid).css("background", "#68686e");
            if (hob != ""){
                $("#" + hob).css("background", "white");
            }
            hob = hid;
            send_hob = parseInt(hid.substring(3));
        }
    });


    var curr_shop = {
        "position": "relative",
        "width": "170px",
        "height": "170px",
        "margin-top": "15px",
        "margin-left": "15px",
        "margin-right": "15px",
        "margin-bottom": "15px",
        "overflow": "hidden",
        "opacity": "0.8",
        "background": "url(h5/img/img-2.png) center no-repeat",
        "background-color": "#9c9c9c",
        "float": "left"
    };

    var new_shop = {
        "position": "relative",
        "width": "690px",
        "height": "200px",
        "margin-top": "15px",
        "margin-bottom": "15px",
        "background-color": "#eeeeee",
        "border": "solid 1px #eee",
        "overflow": "hidden",
        "float": "left"
    };

    var pre_shop = {
        "position": "relative",
        "width": "680px",
        "height": "200px",
        "left": "6%",
        "margin-top": "15px",
        "margin-bottom": "15px",
        "background-color": "#eeeeee",
        "border": "solid 1px #eee",
        "overflow": "hidden",
        "float": "left"
    };

    var good_title = {
        "position": "relative",
        "width": "350px",
        "height": "60px",
        "top": "-15px",
        "float": "left",
        "overflow": "hidden"
    };

    var good_price = {
        "position": "relative",
        "width": "350px",
        "height": "30px",
        "top": "-10px",
        "float": "left",
        "overflow": "hidden"
    };

    var good_intro = {
        "position": "relative",
        "width": "350px",
        "height": "100px",
        "float": "left",
        "line-height": "20px",
        "overflow": "hidden"
    };

    var received_goods = "";
    var goods = {good: []};

    $.ajax({
        url: "https://shoppingmall.mouchina.com/m/itemSearch/solrSearchItem.do?sortType=comments&poem=poem",
        type: "GET",
        dataType: "jsonp",
        success: function(data){
            received_goods = data.data;
            //console.log(received_goods);
            for (var key in received_goods.rows) {
                //console.log(received_goods.rows[key]);
                var good_info = {
                    sku_id: received_goods.rows[key].skuId,
                    seller_id: received_goods.rows[key].sellerId,
                    good_id: received_goods.rows[key].itemId,
                    good_name: received_goods.rows[key].title,
                    good_price: received_goods.rows[key].price,
                    good_pic: good_pic_url + received_goods.rows[key].picUrl,
                    good_intro: "",
                    good_url: ""
                };
                goods.good.push(good_info);
                //console.log(good_info);
            }
            init_goods();
        },
        error: function (data) {
            console.log("error:" + data);
        }
    });

    var good_list_id = "";
    var shop_good = [];
    //将商品读入右侧商品列表 good_list_*：表示左侧商品列表排列顺序
    var goods_list = [];
    var to_list = function() {
        for (var id = 0; id < goods.good.length; id++) {
            var good_name = goods.good[id].good_name;
            good_list_id = "good_list_id_" + id;
            if (good_name.length > 8){
                goods_list.push("<ul class='list-group'><li class='list-group-item lg' id=" + good_list_id + ">" + good_name.substring(0, 8) + "</li></ul>");
            } else {
                goods_list.push("<ul class='list-group'><li class='list-group-item lg' id=" + good_list_id + ">" + good_name + "</li></ul>");
            }
        }
        return goods_list;
    };

    //分页显示商品
    var page_size = 8; //每页商品条目
    var start = 0; //开始下标
    var goods_page = function(goods_list, page_size, id){
        for (var i = 0; id < goods_list.length; i++, id++){
            if (i != page_size){
                $(".goods-list").append(goods_list[id]);
                start = id;
            } else{
                start = id;
                break;
            }
        }
    };

    //初始显示
    goods_page(to_list(), page_size, start);
    //goods_page(to_list(), page_size, 5);

/*    var goods_return = setInterval(function () {
        if (received_goods != "") {
            for (var key in received_goods.rows) {
                //console.log(received_goods.rows[key]);
                var good_info = {
                    good_id: received_goods.rows[key].itemId,
                    good_name: received_goods.rows[key].title,
                    good_price: received_goods.rows[key].price,
                    good_pic: good_pic_url + received_goods.rows[key].picUrl,
                    good_intro: ""
                };
                goods.good.push(good_info);
                //console.log(good_info);
            }
            goods_page(to_list(), page_size, start);
        }
        clearInterval(goods_return);
    }, 1000);*/

    //将选中商品添加到商品展示框
    var gid = ""; //下标，用来同步evt队列good_gid和goods.good[gid]，另外remb_good.goods[gid]同good_gid一样
    var sid = 0; //商店编号
    var new_shop_id = "";//商店编号
    //gid:商品id，也是remb_good.goods[id]
    var show_shop = function(sid){
        var title = 'title_' + sid;
        //var link = 'link_' + gid;
        var price = 'price_' + sid;
        var up = 'upload_shop_' + sid;
        var intro = 'intro_' + sid;
        new_shop_id = "new_shop_" + sid;

        return "<div class=" + new_shop_id + ">" +
            "<div class='intro-img'>" +
            "<a href=" + goods.good[gid].good_url + "><div class=" + up + "></div></a>" +
            "</div>" +
            "<div class='intro-info'>" +
            "<div class='shop-info'>" +
            "<div id=" + title + "></div>" +
            "<div id=" + price + "></div>" +
            "<div id=" + intro + "></div>" +
            "</div></div></div>"
    };

    var show_good = function(gid, new_shop){ //new_shop: $("." + new_shop_id).css(new_shop);

        $("#title_" + sid).html("<h4><a href=" + goods.good[gid].good_url + ">" + goods.good[gid].good_name + "</a></h4>");
        $("#title_" + sid).css(good_title);
        $("#price_" + sid).css(good_price);
        $("#intro_" + sid).css(good_intro);
        //$("#link_" + sid).val(goods.good[gid].good_url);
        $("#price_" + sid).html("<h4>￥ " + goods.good[gid].good_price + "</h4>");
        $("#intro_" + sid).html(goods.good[gid].good_intro);
        $(".upload_shop_" + sid).css(curr_shop);
        $(".upload_shop_" + sid).css({
            "background": "url(" + goods.good[gid].good_pic + ") center",
        });
        shop_good.push({gid: gid, sid: sid});//通过商品编号查找所在商店
        /*for (var i = 0; i < shop_good.length; i++){
            alert(shop_good[i].gid + ": " + shop_good[i].sid);
        }*/
        sid = sid + 1; //新的商店
        add = 0;
        change_high();
    };

    var member_goods = function(light_goods, good_list_id){
        for (var i = 0; i < light_goods.length; i++){
            if (good_list_id == light_goods[i]){
                return true;
            }
        }
        return false;
    };

    var del_member = function(light_goods, good_list_id){
        var res = [];
        for (var i = 0; i < light_goods.length; i++){
            if (good_list_id != light_goods[i]){
                res.push(light_goods[i]);
            }
        }
        return res;
    };

    var remove_arr_item = function(arr, index){
        var res = [];
        for (var i = 0; i < arr.length; i++){
            if (i != index){
                res.push(arr[i]);
            }
        }
        return res;
    };

    var add_or_del_good = function(){
        if (member_goods(light_goods, good_list_id)){
            var curr_gid = parseInt(good_list_id.substring(13));
            light_goods = del_member(light_goods, good_list_id);//删除light_goods
            $("#" + good_list_id).css("background-color", "white");
            //删除商店
            for (var i = 0; i < shop_good.length; i++){
                if (shop_good[i].gid == curr_gid){
                    $(".new_shop_" + shop_good[i].sid).remove();
                }
            }
            //删除remb_good
            for (var i = 0; i < remb_good.length; i++){
                if (remb_good[i].gid == curr_gid){
                    remb_good = remove_arr_item(remb_good, i);
                }
            }
            //删除light_goods
/*
            for (var i = 0; i < light_goods.length; i++){
                if (light_goods[i] == good_list_id){
                    light_goods = remove_arr_item(light_goods, i);
                }
            }
*/
            //删除evt
            for (var i = 0; i < evt_lst.length; i++){
                if (evt_lst[i].substring(0, 4) == "good"){
                    if (evt_lst[i].substring(5) == good_list_id.substring(13)){
                        evt_lst = remove_arr_item(evt_lst, i);
                    }
                }
            }

            change_high();
        } else {
            $("#" + good_list_id).css("background-color", "#eee");
            light_goods.push(good_list_id);
            //alert(light_goods);
            gid = good_list_id.substring(13);
            evt_lst.push("good_" + gid); //添加商品id进入队列
            remb_good.push({gid: gid, good: goods.good[gid]});
            $(".link").append(show_shop(sid));//画出商店
            show_good(gid, $("." + new_shop_id).css(new_shop));//展示商品
        }
    };

    var light_goods = []; //被选中的商品
    $(".goods-list ul li").click(function(){
        good_list_id = $(this).attr("id");
        //alert(good_list_id);
        add_or_del_good();
    });

    var init_goods = function(){
        if (start >= to_list().length - 1) {
            $(".goods-list ul").remove();
            goods_page(to_list(), page_size, 0);
            light_goods.map(function (e) {
                $("#" + e).css("background-color", "#eee")
            });
        } else {
            $(".goods-list ul").remove();
            goods_page(to_list(), page_size, start);
            light_goods.map(function (e) {
                $("#" + e).css("background-color", "#eee")
            });
        }

        $(".goods-list ul li").click(function () {
            good_list_id = $(this).attr("id");
            //alert(good_list_id);
            add_or_del_good();
        });
    };

    $(".bgr").click(function() {
        if (start >= to_list().length - 1) {
            $(".goods-list ul").remove();
            goods_page(to_list(), page_size, 0);
            light_goods.map(function (e) {
                $("#" + e).css("background-color", "#eee")
            });
        } else {
            $(".goods-list ul").remove();
            goods_page(to_list(), page_size, start);
            light_goods.map(function (e) {
                $("#" + e).css("background-color", "#eee")
            });
        }

        $(".goods-list ul li").click(function () {
            good_list_id = $(this).attr("id");
            //alert(good_list_id);
            add_or_del_good();
        });
    });


    $(".bgl").click(function() {
        if (start >= to_list().length - 1) {
            $(".goods-list ul").remove();
            goods_page(to_list(), page_size, 0);
            light_goods.map(function (e) {
                $("#" + e).css("background-color", "#eee")
            });
        } else {
            $(".goods-list ul").remove();
            goods_page(to_list(), page_size, start);
            light_goods.map(function (e) {
                $("#" + e).css("background-color", "#eee")
            });
        }

        $(".goods-list ul li").click(function () {
            good_list_id = $(this).attr("id");
            //alert(good_list_id);
            add_or_del_good();
        });
    });

    ////////////////////////////////////////////////////////////////////////////////

    var img_upload_css = {
        "position": "relative",
        "width": "150px",
        "height": "150px",
        "border": "dashed 1px #c4c4c4",
        "margin-top": "20px",
        "margin-left": "25px",
        "margin-right": "15px",
        "margin-bottom": "25px",
        "overflow": "hidden",
        "opacity": "0.5",
        //"background": "url(h5/img/img-1.png) center no-repeat",
        "background": "url(img/img-1.png) center no-repeat",
        "background-color": "#9c9c9c",
        "float": "left"
    };

    var curr_image_css = {
        "position": "relative",
        "height": "150px",
        "overflow": "hidden",
        "opacity": "0",
        "cursor": "pointer"
    };

    var curr_pic_css = {
        "position": "relative",
        "width": "150px",
        "height": "150px",
        "border": "dashed 1px #c4c4c4",
        "margin": "20px 15px 25px 25px",
        "overflow": "hidden",
        "float": "left",
        "z-index": "1"
    };

    var curr_close_css = {
        "position": "relative",
        "width": "30px",
        "height": "30px",
        "border": "solid 1px #c4c4c4",
        "border-radius": "20px",
        "background": "url(h5/img/colse.png) center no-repeat",
        "background-color": "#ffffff",
        "float": "right",
        "opacity": "0.8",
        "z-index": "5",
        "text-align": "center",
        "font-size": "small",
        "padding-top": "6px",
        "padding-left": "1px",
        "color": "red",
        "cursor": "pointer"
    };

    /*var compPic;
    var compress = function() {
        $('.image').imageCompress({
            'quality': 50,
            'onloadStart': function (result) {
                //console.log('读取图片开始' + result);
            },
            'onloadEnd': function (result) {
                //console.log('读取图片结束' + result);
            },
            'oncompressStart': function (result) {
                //console.log('压缩图片开始' + result);
            },
            'oncompressEnd': function (result) {
                console.log('压缩图片结束' + result);
                $('#preview').append(result);
                compPic = $('#preview').find('img').attr("src");
            },
            'callback': function () {
                //console.log('处理完毕');
            }
        });
    };

    compress();

    function dataURItoBlob(base64Data) {
        var byteString;
        if (base64Data.split(',')[0].indexOf('base64') >= 0)
            byteString = atob(base64Data.split(',')[1]);
        else
            byteString = unescape(base64Data.split(',')[1]);
        var mimeString = base64Data.split(',')[0].split(':')[1].split(';')[0];
        var ia = new Uint8Array(byteString.length);
        for (var i = 0; i < byteString.length; i++) {
            ia[i] = byteString.charCodeAt(i);
        }
        return new Blob([ia], {type:mimeString});
    }*/

    /*var upload = function (formData) {
        $.ajax({
            //url: req_upload_url ,
            url: "http://localhost:8100/upload",
            type: 'POST',
            data: formData,
            async: true,
            cache: false,
            contentType: false,
            processData: false,
            success: function (data) {
                //var data_url = JSON.parse(data).image.url;
                var data_url = data; //myserver

                var class_id = "show_upload_0_img_id_" + cnt_pic;
                var close_id = "close_" + cnt_pic;

                $(".show_upload").append("<div class=" + class_id + ">" +
                    "<div class=" + close_id + ">" +
                    "<span class=\"glyphicon glyphicon-remove\" aria-hidden=\"true\"></span>" +
                    "</div>" +
                        //"<img src=" + remb_pic[cnt_pic] + " width='150'/>" +
                    "</div>");

                var len = document.getElementById("file").files.length;
                console.log(len);

                for (var i = 0; i < len; i++) {
                    var file = document.getElementById("file").files[i];
                    var reader = new FileReader();
                    reader.readAsDataURL(file);

                    reader.onload = function (e) {
                        var result = document.getElementById("file");
                        //result.innerHTML = '<img src="' + this.result + '" alt="" />';
                        remb_pic.images[cnt_pic] = {img_id: class_id, img_url: data_url}; //this.result}; //data_url
                        $("." + class_id).css("background", "url(" + this.result + ") no-repeat center");
                        $("." + close_id).click(function () {
                            $("." + class_id).remove();
                            var index = parseInt(class_id.substring(21));
                            delete remb_pic.images[index];
                            curr_pic_cnt = curr_pic_cnt - 1;
                            change_high();
                        });
                        cnt_pic = cnt_pic + 1;
                        curr_pic_cnt = curr_pic_cnt + 1;
                    };

                    $("." + class_id).css(curr_pic_css);
                    $("." + close_id).css(curr_close_css);
                    change_high();
                }
            },
            error: function (data) {
                console.log("error:" + data);
            }
        });
    };

    var sum = 0;

    $(".image-upload").change(function () {
        if (curr_pic_cnt < 5) {
            var check = setInterval(function () {
                if (compPic != undefined) {
                    clearInterval(check);
                    var formData = new FormData($("#formdata")[0]);
                    var len = document.getElementById("file").files.length;

                    for (var i = 0; i < len; i++){
                        var blob = dataURItoBlob(compPic);
                        var fd = new FormData();
                        fd.append("files", blob);
                        upload(formData);
                    }
                    change_high();
                }
            }, 100);
        }
    });*/

    var showPic = function(data_url){
        var class_id = "show_upload_0_img_id_" + cnt_pic;
        var close_id = "close_" + cnt_pic;

        $(".show_upload").append("<div class=" + class_id + ">" +
            "<div class=" + close_id + "></div></div>");

        var len = document.getElementById("file").files.length;

        for (var i = 0; i < len; i++){
            var file = document.getElementById("file").files[i];
            $("#file").val('');
            var reader = new FileReader();
            reader.readAsDataURL(file);

            reader.onload=function(e) {
                var result = document.getElementById("file");
                //result.innerHTML = '<img src="' + this.result + '" alt="" />';
                remb_pic.images[cnt_pic] = {img_id: class_id, img_url: data_url}; //this.result}; //data_url
                console.log(remb_pic);
                $("." + class_id).css("background", "url(" + this.result + ") no-repeat center");
                $("." + close_id).click(function () {
                    $("." + class_id).remove();
                    var index = parseInt(class_id.substring(21));
                    delete remb_pic.images[index];
                    curr_pic_cnt = curr_pic_cnt - 1;
                    change_high();
                });
                cnt_pic = cnt_pic + 1;
                curr_pic_cnt = curr_pic_cnt + 1;
                console.log(curr_pic_cnt)
            };

            $("." + class_id).css(curr_pic_css);
            $("." + close_id).css(curr_close_css);
            change_high();
        }
    };

    $(".image-upload").change(function () {
        if (curr_pic_cnt < 5){
            var data_url = "";
            var formData = new FormData($("#formdata")[0]);
            $.ajax({
                //url: req_upload_url ,
                url: "http://localhost:8100/upload",
                type: 'POST',
                data: formData,
                async: true,
                cache: false,
                contentType: false,
                processData: false,
                success: function (data) {
                    data_url = data;
                    //data_url = JSON.parse(data).image.url;
                    showPic(data_url);
                },
                error: function (data) {
                    console.log("error:" + data);
                }
            });
            change_high();
        }
    });

    ///////////////////////////////////////////////////////////////////////////////////////

    var shop = [];
    var show_upload = ['show_upload_0_']; //添加显示图片的区域
    var show_cnt = 1;
    var img_id = ['image'];
    var img_cnt = 1;
    var img_upload_id = ['image-upload'];
    var img_up_cnt = 1;

    $("#add_img").click(function(){
        if (add == 0) {
            show_upload[show_cnt] = "show_upload_" + show_cnt; //show_cnt目前为1位，需要处理1位以上
            img_id[img_cnt] = "image_" + img_cnt;
            img_upload_id = "image-upload-" + img_up_cnt;
            var form_id = "formdata_" + img_up_cnt;
            var file_id = "file_" + img_up_cnt;
            evt_lst.push(show_upload[show_cnt]);
            var img_upload_name = "." + img_upload_id;
            var curr_pic_cnt = 0; ////限制图片上传数量，不能超过5

            $(".link").append(
                "<div class=" + show_upload[show_cnt] + "></div>" +
                "<div class=" + img_upload_id + ">" +
                "<form id=" + form_id + ">" +
                "<input name=\"files\" type=\"file\" multiple=\"multiple\" class=" + img_id[img_cnt] + " id=" + file_id + " /></form></div>");

            var img_name = "." + img_id[img_cnt];
            var show_name = "." + show_upload[show_cnt];
            var show_path = show_upload[show_cnt];

            $(img_upload_name).css(img_upload_css);
            $(img_name).css(curr_image_css);

            $(img_upload_name).change(function () {
                if (curr_pic_cnt < 5) {
                    var data_url = "";
                    var formData = new FormData($("#" + form_id)[0]);

                    $.ajax({
                        //url: req_upload_url ,
                        url: "http://localhost:8100/upload",
                        type: 'POST',
                        data: formData,
                        async: true,
                        cache: false,
                        contentType: false,
                        processData: false,
                        success: function (data) {
                            data_url = data;
                            //data_url = JSON.parse(data).image.url;

                            var class_id = show_path + "_img_id_" + cnt_pic;
                            var close_id = "close_" + cnt_pic;
                            $(show_name).append("<div class=" + class_id + ">" +
                                "<div class=" + close_id + "></div></div>");
                            var class_name = "." + class_id;
                            var close_name = "." + close_id;

                            var len = document.getElementById(file_id).files.length;
                            console.log(len);

                            for (var i = 0; i < len; i++){
                                var file = document.getElementById(file_id).files[0];
                                $("#" + file_id).val('');
                                var reader = new FileReader();
                                reader.readAsDataURL(file);

                                reader.onload=function(e) {
                                    var result = document.getElementById(file_id);
                                    remb_pic.images[cnt_pic] = {img_id: class_id, img_url: data_url};//this.result};
                                    $("." + class_id).css("background", "url(" + this.result + ") no-repeat center");

                                    $(close_name).click(function () {
                                        $(class_name).remove();
                                        if ((show_cnt + '').length == 1){
                                            var index = parseInt(class_id.substring(21)); //未来用变量
                                        } else if ((show_cnt + '').length == 2){
                                            var index = parseInt(class_id.substring(22)); //未来用变量
                                        } //暂未处理三位数
                                        delete remb_pic.images[index];
                                        curr_pic_cnt = curr_pic_cnt - 1;
                                        change_high();
                                    });
                                    cnt_pic = cnt_pic + 1;
                                    curr_pic_cnt = curr_pic_cnt + 1;
                                };
                                $(class_name).css(curr_pic_css);
                                $(close_name).css(curr_close_css);
                                change_high();
                            }
                        },
                        error: function (data) {
                            alert("error:" + data);
                        }
                    });
                }
            });
            change_high();
            img_cnt = img_cnt + 1;
            img_up_cnt = img_up_cnt + 1;
            show_cnt = show_cnt + 1;
            add = 1; //不允许在本段落操作中再次点击添加图片按钮
        }
    });

    /**********************************************************************************************/

    var words_count = function(eid, rid){
        var cont_words = 0;
        $("#" + eid).keyup(function() {
            cont_words = $("#" + eid).val();
            $("#" + rid).html("<p id=\"cnt\">" + cont_words.length + "/5000</p>");
        });
    };

    var words = 0;
    $("#auth").keyup(function() {
        words = $('#auth').val();
        $("#count-auth").html("<p>" + words.length + "/25</p>")
    });

    var cont_words = 0;
    $("#text_0").keyup(function() {
        cont_words = $("#text_0").val();
        $("#count-cont").html("<p id=\"cnt\">" + cont_words.length + "/5000</p>")
    });

    ////////////////////////////////////////////////////////////////////////////////

    var art_id = ['text'];
    var art_cnt = 1;
    $("#add_art").click(function(){
        art_id[art_cnt] = "text_" + art_cnt;
        //count[art_cnt] = "count_cnt_" + art_cnt;
        evt_lst.push(art_id[art_cnt]);
        remb_text.text.contents[art_cnt] = {text_id: art_id[art_cnt], content: ""};
        $(".link").append(
            "<textarea name=\"content\" type=\"text\" class=\"init-text\" id=" + art_id[art_cnt] + " size=\"30\" maxlength=\"5000\"" +
            "style='resize:none;overflow-x:hidden;overflow-y:visible;font-size:medium;width:100%;height:300px;margin-top:25px;margin-bottom:25px;color:#363636' ></textarea>");
            //"<div id=" + count[art_cnt] + "><p id='cnt'>0/5000</p></div>");
        art_cnt = art_cnt + 1;
        change_high();
        add = 0;
    });

    ////////////////////////////////////////////////////////////////////////////////

    $("#final").click(function(){
        var text_val = $("#text_0").val();
        remb_text.text.title = $("#title").val();
        remb_text.text.author = $("#auth").val();

        if($("#summary").val() == ""){
            var summary = text_val.substring(0, 200);
            remb_text.text.summary = summary;
        } else {
            remb_text.text.summary = $("#summary").val();
        }

        if(remb_text.text.title == ""){
            alert('请输入文章标题');
            return;
        }

        if(text_val == ""){
            alert('请输入文章');
            return;
        }

        var check_imgs_null = function(){
            if (remb_pic.images.length == 0){
                return true;
            } else {
                for (var i = 0; i < remb_pic.images.length; i++) {
                    if (remb_pic.images[i] != null) {
                        preferredPic = remb_pic.images[i].img_url;
                        return false;
                    }
                }
            }
        };

        if (check_imgs_null()){
            alert('请至少上传一张图片');
            return;
        }

        remb_text.text.contents[0] = {text_id: 'text_0', content: text_val};

        for(var i = 0; i < remb_text.text.contents.length; i++){
            remb_text.text.contents[i].content = $("#" + remb_text.text.contents[i].text_id).val();
        }

        var send_data = {
            title: remb_text.text.title,
            preferredPic: preferredPic,
            labelType: send_hob,
            contents: JSON.stringify([evt_lst, remb_text.text, remb_pic.images, remb_good])
        };

        var send_str = JSON.stringify(send_data);

        if (send_str.length > 5000){
            alert("编辑内容过长，请删减后提交");
            return;
        }

        if (remb_text.text.title != "" && text_val != "" && !check_imgs_null() && send_hob != "") {
            $.ajax({
                //url: req_submit_url,
                url: "http://localhost:8400/poem",
                //async: false,
                type: "POST",
                data: send_data,
                success: function (data) {
                    //console.log(data);
                    if(data === "success"){
                        alert("提交成功");
                        window.location.reload();
                    }else{
                        alert("提交失败,请重新提交");
                    }
                },
                error: function (xhr) {
                    alert("服务器异常,请尝试重新提交");
                }
            });
        }
    });

    //预览
    var pre_lock = 1; //防止多次点击生成新节点
    $(".t").click(function(){
        if (pre_lock == 1) {
            $(".middle-panel").append("<div class='preview'></div>");
            $("#add-text").hide();
            $("#edit-text").hide();
            $(".goods-list").hide();
            $(".article-list").hide();
            $(".title").hide();
            $(".contents").hide();
            $(".bt-art-lr").hide();
            $(".bt-goods-lr").hide();
            $(".hide-show-left").hide();
            $(".hide-show-right").hide();
            //$(".left-panel").show();
            //$(".middle-panel").css("width", "768px");

            var text_val = $("#text_0").val();
            remb_text.text.title = $("#title").val();
            remb_text.text.author = $("#auth").val();

            if($("#summary").val() == ""){
                var summary = text_val.substring(0, 200);
                remb_text.text.summary = summary;

            } else {
                remb_text.text.summary = $("#summary").val();
            }

            document.getElementsByName("summary").innerText = summary;

            remb_text.text.contents[0] = {text_id: 'text_0', content: text_val};

            for (var i = 0; i < remb_text.text.contents.length; i++) {
                remb_text.text.contents[i].content = $("#" + remb_text.text.contents[i].text_id).val();
            }

            make_pre();
            $(".preview").show();
            $(".p").css("background-color", "white");

            if ($(".middle-panel").height() < 1100){
                $(".left-panel").css("height", "1100px");
                $(".middle-panel").css("height", "1100px");
                $(".right-panel").css("height", "1100px");
            } else {
                $(".left-panel").css("height", "");
                $(".middle-panel").css("height", "");
                $(".right-panel").css("height", "");
            }

            change_high();
            //console.log("preview:" + $(".middle-panel").height());
            pre_lock = 0;
        }
    });

    //编辑
    $(".p").css("background-color", "#eee");
    $(".p").click(function(){
        $(".left-panel").css("height", "");
        $(".middle-panel").css("height", "");
        $(".right-panel").css("height", "");

        $(".p").css("background-color", "#eee");
        $("#add-text").show();
        $(".goods-list").show();
        $("#edit-text").show();
        $(".article-list").show();
        $(".title").show();
        $(".contents").show();
        $(".bt-art-lr").show();
        $(".bt-goods-lr").show();
        $(".hide-show-left").show();
        $(".hide-show-right").show();
        $(".preview").remove();
        change_high();
        pre_lock = 1;
    });

    //text:
    var show_title = function(){
        return "<div class='new_title'><h2>" + remb_text.text.title + "</h2>" +
            "<h4>作者: " + remb_text.text.author + "</h4>" +
            "<pre class='smry'>" + remb_text.text.summary + "</pre></div>"
    };

    var show_text = function(tid){
        //return "<div class='txt'><p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"  + remb_text.text.contents[tid].content + "</p></div>";
        return "<div class='txt'><pre class='format'>"  + remb_text.text.contents[tid].content + "</pre></div>";
    };

    //生成预览
    var make_pre = function(){
        //必须显示
        $(".preview").append(
            show_title() +
            show_text(0)
        );
        //按组显示上传照片
        for (var i = 0; i < remb_pic.images.length; i++) {
            if (remb_pic.images[i] != null) {
                if (remb_pic.images[i].img_id.substring(12, 13) == 0) {
                    $(".preview").append("<div class='imgs'><img src=" + remb_pic.images[i].img_url + " width='680' /></div>");
                    change_high();
                }
            }
        }

        var evt = evt_lst;

        if(evt != "") {
            for (var ev in evt) {
                if (evt_lst[ev].substring(0, 4) == "text") { //文本
                    var tid = parseInt(evt_lst[ev].substring(5));
                    //console.log(tid);
                    var cont = $("#" + remb_text.text.contents[tid].text_id).val();
                    if (cont != ""){
                        remb_text.text.contents[tid].content = $("#" + remb_text.text.contents[tid].text_id).val();
                        $(".preview").append(show_text(parseInt(evt_lst[ev].substring(5))));
                    }
                } else
                if (evt_lst[ev].substring(0, 4) == "show") { //图片
                    var flag = evt_lst[ev].substring(12); //获取show_upload_*
                    for (var i = 0; i < remb_pic.images.length; i++) {
                        if (remb_pic.images[i] != null) {
                            if (remb_pic.images[i].img_id.substring(12).match(/\d+/g)[0] == flag) {
                                $(".preview").append("<div class='imgs'><img src=" + remb_pic.images[i].img_url + " width='680' /></div>");
                            }
                        }
                    }
                } else
                if (evt_lst[ev].substring(0, 4) == "good"){ //商品
                    $(".preview").append(show_shop(sid));
                    show_good(evt_lst[ev].substring(5), $(".new_shop_" + sid).css(pre_shop));
                }
            }
        }
    };

    var edit = function(eid) {
        $.ajax({
            url: "http://localhost:8400/poem",
            dataType: "json",
            //url: projectUrl() + "/advert/poem/get.json?poemAdvertId=" + eid,
            success: function(data){
                //将data展示出来
                var edit_lst = data[0]; //文章元素（文字段落，图片集合，商品集合）顺序表，以此来提取各元素在文章中的位置顺序
                var edit_text = data[1]; //文章段落，包含文章中全部文字，title,author（从服务器获得）,summary,labelType可以忽略，因为已经显示在标题页面了
                var edit_pic = data[2]; //图片集合，通过分析img_id将图片显示在正确位置 img_url是服务器图片地址
                var edit_good = data[3]; //商品集合，gid的value和evt_lst中good_id中的id相同，good集合中包含该商品需要显示的一切信息，有商品good_id，这是存储在服务器中的商品唯一id，
                console.log(edit_lst);
                console.log(edit_text);
                console.log(edit_pic);
                console.log(edit_good);

                //显示图片
                var arr_id = []; //从文章列表中读取的图片id集合
                var showImg = function(index){
                    var curr_pic_cnt = 0;
                    for (var i = 0; i < edit_pic.length; i++) {
                        if (edit_pic[i] != null) {
                            if (edit_pic[i].img_id.substring(12, 13) == index) {
                                var class_id = "show_upload_" + index + "_img_id_" + cnt_pic;
                                var close_id = "close_" + cnt_pic;
                                arr_id.push(cnt_pic);
                                remb_pic.images[cnt_pic] = {img_id: class_id, img_url: edit_pic[i].img_url};
                                //console.log(remb_pic);
                                if (index == 0){
                                    $(".show_upload").append("<div class=" + class_id + ">" +
                                        "<div class=" + close_id + "></div></div>");
                                } else {
                                    $(".show_upload_" + index).append("<div class=" + class_id + ">" +
                                        "<div class=" + close_id + "></div></div>");
                                }
                                $("." + class_id).css("background", "url(" + edit_pic[i].img_url + ") no-repeat center");
                                cnt_pic = cnt_pic + 1;
                                curr_pic_cnt = curr_pic_cnt + 1;
                                //console.log(curr_pic_cnt);
                                $("." + class_id).css(curr_pic_css);
                                $("." + close_id).css(curr_close_css);
                                change_high();
                            }
                        }
                    }
                };

                //删除图片
                var delImg = function(class_name, index){
                    $(class_name).click(function (e) {
                        var id = $(e.target).attr("class");
                        //console.log(id);
                        if (id.substring(0,5) == "close"){
                            var num = id.substring(6);
                            if (member(arr_id, num)){ //只删除可编辑的图片，不包含新添加的图片
                                $("." + "show_upload_" + index + "_img_id_" + num).remove();
                                delete remb_pic.images[num];
                                curr_pic_cnt = curr_pic_cnt - 1;
                                //console.log(curr_pic_cnt);
                                change_high();
                            }
                        }
                    });
                };

                $("#title").val(edit_text.title);
                $("#text_0").val(edit_text.contents[0].content);

                //第一组图片
                showImg(0);

                delImg(".show_upload", 0);

                change_high();

                var otherInfo = function(){
                    if(edit_lst != "") {
                        for (var ev in edit_lst) {
                            if (edit_lst[ev].substring(0, 4) == "text") { //文本
                                var tid = parseInt(edit_lst[ev].substring(5)); //tid是段落id，即evt_lst中text_1中的1
                                var curr_text = edit_text.contents[tid].content; //取出该段落
                                $(".link").append(
                                    "<textarea name=\"content\" type=\"text\" class=\"init-text\" id=" + edit_lst[ev] + " size=\"30\" maxlength=\"5000\"" +
                                    "style='resize:none;overflow-x:hidden;overflow-y:visible;font-size:medium;width:100%;height:300px;margin-top:25px;margin-bottom:25px;color:#363636' ></textarea>");
                                $("#" + edit_lst[ev]).val(curr_text);
                                add = 0;
                            }

                            if (edit_lst[ev].substring(0, 4) == "show") { //图片
                                var flag = edit_lst[ev].substring(12); //获取show_upload_*，*是该图片所属的区域，因为一个区域会有多张图片的可能
                                showImg(flag);
                                add = 1; //不允许在本段落操作中再次点击添加图片按钮
                            }

                            if (edit_lst[ev].substring(0, 4) == "good"){ //商品
                                var id = edit_lst[ev].substring(5); //从evt_lst中获取商品gid
                                console.log(id);
                                $(".link").append(show_shop(sid));//画出商店
                                show_good(id);//展示商品
                            }
                        }
                        change_high();
                    }
                };
                otherInfo();
            },
            error: function(xhr){
                console.log(xhr);
            }
        })
    };
});

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////