/**
 * Created by Administrator on 2017/4/20 0020.
 */

$(document).ready(function(){
    var url = "https://changangw.mouchina.com";
    var loginKey;
    var userId;
    var registType = 1; //正常注册

    var tusitemp="";
    function mess_tusi(strs){
        //清除事件
        clearTimeout(tusitemp);
        $("#mess_tusi").remove();
        //创建吐丝层并写入内容
        if(!$("#mess_tusi").attr("id")){ //吐丝层不存在创建
            $("body").append("<div id='mess_tusi' style='z-index: 100002;position:fixed;font-size:16px;border-radius:4px !important;background:rgba(0,0,0,.7);color:#fff;display:none;'><span style='display:block;padding:5px 15px;'>"+strs+"</span></div>"); //写入内容
        }else{
            $("#mess_tusi").html(strs);  //写入内容
        }

        //定义吐丝层位置
        var left=($(window).width()-$("#mess_tusi").width())/2;//居中
        //var top=($(window).height()-$("#mess_tusi").height())/2;//居中
        var top=$(window).height()*0.8;//偏下
        $("#mess_tusi").css({"left":left+"px","top":top+"px"});

        //显示吐丝层
        $("#mess_tusi").css("display",'');

        //2秒后关闭
        tusitemp =  setTimeout(function (){
            $("#mess_tusi").remove();
            $("#mess_tusi").html("");
        },2000);
        return false;
    }

    //eye's status
    var mieru = true;
    $(".eye").click(function(){
        if (mieru){
            $(".eye").css({"background": "url(../img/icon_password_close2x.png) no-repeat center",
                           "background-size": "100% 100%"});
            $(".pass").attr("type", "text");
            mieru = false;
        } else {
            $(".eye").css({"background": "url(../img/icon_password_visible2x.png) no-repeat center",
                           "background-size": "100% 100%"});
            $(".pass").attr("type", "password");
            mieru = true;
        }
    });

    //获取验证码
    var phoneNum;
    var codeSuccess = false;
    var timeout = false;
    var seconds = 60;
    $(".check-num").click(function(){
        if (!timeout){
            timeout = true;
            phoneNum = $(".phone").val();
            var regexp = /^(13|15|17|18)/;
            var reg =  new RegExp(regexp);
            if (phoneNum != "" && reg.test(phoneNum) && phoneNum.length == 11) {
                var count = setInterval(function(){
                    if (seconds > 0){
                        seconds -= 1;
                        $(".check-num").text(seconds + "秒");
                    } else {
                        $(".check-num").text("重新获取验证码");
                        timeout = false;
                        seconds = 60;
                        clearInterval(count);
                    }
                }, 1000);
                $.ajax({
                    url: url + "/user/register/phone/check?phone=" + phoneNum,
                    success: function(data){
                        console.log(data);
                        if (data.result === 1){
                            codeSuccess = true;
                            console.log("短信发送成功");
                        } else if (data.result === 2) {
                            console.log("已注册");
                        } else if (data.result === 0) {
                            console.log("失败");
                            clearInterval(count);
                            mess_tusi("获取验证码次数已达到上限");
                            $(".check-num").text("获取验证码").css("color", "grey");
                            timeout = true;
                        }
                    },
                    error: function(xhr){
                        console.log(xhr);
                    }
                });
            } else {
                mess_tusi("请输入正确的手机号码");
                timeout = false;
            }
        }
    });

    //注册
    $(".sub").click(function(){
        var code = $(".code").val();
        var pass = $(".pass").val();

        if (phoneNum != "" && codeSuccess && pass != ""){
            $.ajax({
                url: url + "/user/register?phone=" + phoneNum + "&passWord=" + pass + "&smsCode=" + code + "&type=" + registType,
                success: function(data){
                    if (data.result === 1){
                        mess_tusi("注册成功");
                        //console.log(data);
                        //loginKey = data.loginKey;
                        //userId = data.userId;
						localStorage.setItem("loginKey", data.loginKey);
						localStorage.setItem("phoneNum", phoneNum);
                        window.location.href = "../index.html";
                        //window.location.href = "login.html";
                    } else if (data.result === 0){
                        mess_tusi("注册失败");
                    } else {
                        mess_tusi("该手机号已经注册过了，请直接登录");
                        window.location.href = "login.html";
                    }
                },
                error: function(xhr){
                    console.log(xhr);
                }
            })
        } else {
            mess_tusi("请完善注册信息");
        }
    });

    //返回登录页面
    $(".back").click(function(){
        window.location.href = "login.html";
    })
});