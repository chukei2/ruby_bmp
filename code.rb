# rubocop:disable all
W = 120
H = 80
TABLE = [
"MMMMMM###TTTTTTT",
"QQBMMNW##TTTTTV*",
"QQQBBEK@PTTTVVV*",
"QQQmdE88P9VVVV**",
"QQQmdGDU0YVV77**",
"pQQmAbk65YY?7***",
"ppgAww443vv?7***",
"pggyysxcJv??7***",
"pggyaLojrt<<+**\"",
"gggaauuj{11!//\"\"",
"gggaauui])|!/~~\"",
"ggaauui]((;::~~^",
"ggaauu](;;::-~~'",
"ggauu(;;;;---~``",
"gaau;;,,,,,...``",
"gau,,,,,,,,...  "]
FONT={
  C:[[[10,4],[8,4],[8,0]],[[7,2],[8,4],[8,0]],[[8,0],[7,2],[2,0]],[[3,2],[7,2],[2,0]],[[2,0],[3,2],[0,4]],[[2,4],[3,2],[0,4]],[[0,4],[2,4],[0,10]],[[2,10],[2,4],[0,10]],[[0,10],[2,10],[2,14]],[[3,12],[2,10],[2,14]],[[2,14],[3,12],[8,14]],[[7,12],[3,12],[8,14]],[[8,14],[7,12],[10,10]],[[8,10],[7,12],[10,10]]],
  H:[[[0,0],[2,0],[0,14]],[[2,14],[2,0],[0,14]],[[8,0],[10,0],[8,14]],[[10,14],[10,0],[8,14]],[[2,8],[2,6],[8,8]],[[8,6],[2,6],[8,8]]],
  U:[[[0,0],[0,0],[2,0]],[[0,12],[0,0],[2,0]],[[2,0],[0,12],[2,12]],[[1,14],[0,12],[2,12]],[[8,0],[8,0],[10,0]],[[8,12],[8,0],[10,0]],[[10,0],[8,12],[10,12]],[[9,14],[8,12],[10,12]]],
  R:[[[0,0],[2,0],[0,14]],[[2,14],[2,0],[0,14]],[[2,0],[2,2],[8,0]],[[7,2],[2,2],[8,0]],[[8,0],[7,2],[10,2]],[[8,3],[7,2],[10,2]],[[10,2],[8,3],[10,6]],[[8,5],[8,3],[10,6]],[[10,6],[8,5],[8,8]],[[7,6],[8,5],[8,8]],[[8,8],[7,6],[2,8]],[[2,6],[7,6],[2,8]],[[6,8],[8,8],[8,14]],[[10,14],[8,8],[8,14]]],
  A:[[[0,14],[4,0],[2,14]],[[6,0],[4,0],[2,14]],[[2,7],[2,9],[8,7]],[[8,9],[2,9],[8,7]],[[8,14],[4,0],[10,14]],[[6,0],[4,0],[10,14]]],
  D:[[[0,0],[2,0],[0,14]],[[2,14],[2,0],[0,14]],[[10,10],[8,10],[10,4]],[[8,4],[8,10],[10,4]],[[10,4],[8,4],[8,0]],[[7,2],[8,4],[8,0]],[[8,0],[7,2],[2,0]],[[3,2],[7,2],[2,0]],[[2,0],[3,2],[0,4]],[[2,4],[3,2],[0,4]],[[0,4],[2,4],[0,10]],[[2,10],[2,4],[0,10]],[[0,10],[2,10],[2,14]],[[3,12],[2,10],[2,14]],[[2,14],[3,12],[8,14]],[[7,12],[3,12],[8,14]],[[8,14],[7,12],[10,10]],[[8,10],[7,12],[10,10]]],
  T:[[[0,0],[10,0],[0,2]],[[10,2],[10,0],[0,2]],[[0,2],[10,2],[4,2]],[[6,2],[10,2],[4,2]],[[4,2],[6,2],[4,14]],[[6,14],[6,2],[4,14]]],
  U:[[[0,0],[2,0],[0,12]],[[2,12],[2,0],[0,12]],[[0,12],[2,12],[2,14]],[[6,12],[2,12],[2,14]],[[2,14],[6,12],[6,14]],[[6,0],[6,12],[6,14]],[[6,14],[6,0],[8,12]],[[8,0],[6,0],[8,12]]],
  B:[[[0,0],[2,0],[0,14]],[[2,14],[2,0],[0,14]],[[2,0],[2,2],[8,0]],[[7,2],[2,2],[8,0]],[[8,0],[7,2],[10,2]],[[8,3],[7,2],[10,2]],[[10,2],[8,3],[10,6]],[[8,5],[8,3],[10,6]],[[10,6],[8,5],[8,8]],[[7,6],[8,5],[8,8]],[[8,8],[7,6],[2,8]],[[2,6],[7,6],[2,8]],[[2,14],[2,12],[8,14]],[[7,12],[2,12],[8,14]],[[8,14],[7,12],[10,12]],[[8,11],[7,12],[10,12]],[[10,12],[8,11],[10,8]],[[8,9],[8,11],[10,8]],[[10,8],[8,9],[8,6]],[[7,8],[8,9],[8,6]],[[8,6],[7,8],[2,6]],[[2,8],[7,8],[2,6]]],
  Y:[[[6,6],[8,6],[6,14]],[[8,14],[8,6],[6,14]],[[0,0],[6,6],[4,0]],[[8,6],[6,6],[4,0]],[[14,0],[8,6],[10,0]],[[6,6],[8,6],[10,0]]],  
}
class Canvas
  attr_reader :width, :height, :color, :depth
  def initialize(w, h)
    @width = w
    @height = h
    @color = height.times.map{width.times.map{1}}
    @depth = height.times.map{width.times.map{999}}
  end
  def clear(z,t,sea=0.4,sky=0.6)
    depth.each{|l|l.map!{999}}
    width.times{|x|
      y1=(Math.sin(x*0.16-t)+Math.sin(x*0.23-t))*2+z*height
      y2=(Math.sin(x*0.14-t)+Math.sin(x*0.16+t))*2+z*height
      y3=(Math.sin(x*0.23+t)+Math.sin(x*0.17+t))*2+z*height
      height.times{|y|
        color[y][x]=sky+(sea-sky)*[y1,y2,y3].count{|yy|y>yy}/3.0
      }
    }
  end
  def camera(x,y,z,vxy,vz)
    @camx=x
    @camy=y
    @camz=z
    @rotxy = Math::E**(Math::PI/2-vxy).i
    @rotz = Math::E**(Math::PI/2-vz).i
  end

  def render(t)
    width.times{|ix|height.times{|iy|
      x=(2.0*ix-width)/height
      y=(2.0*iy-height)/height
      color[iy][ix]=(t+x*x+y*y+2*Math.sin(5*Math.atan2(y,x)+t))/3%1
    }}
  end
  def ball(x,y,z,r,c)
    x,y=((x-@camx+(y-@camy).i)*@rotxy).rect
    y,z=((y+(z-@camz).i)*@rotz).rect
    return if z<=0
    x*=2/z
    y*=2/z
    r*=2/z
    cx=width*(1+x)*0.5
    cy=(y*width+height)*0.5
    cr=r*width*0.5
    ([cx-cr,0].max.ceil..[cx+cr,width-1].min.floor).each{|ix|
      ([cy-cr,0].max.ceil..[cy+cr,height-1].min.floor).each{|iy|
        if (ix-cx)**2+(iy-cy)**2<cr*cr && depth[iy][ix]>z
          depth[iy][ix]=z
          color[iy][ix]=c
        end
      }
    }
  end

  def triangle(a, b, c)
    return if [a, b, c].any?(&:nil?)  # nilチェックを追加
    a,b,c=[a,b,c].map{|p|
      x,y=((p[0]-@camx+(p[1]-@camy).i)*@rotxy).rect
      y,z=((y+(p[2]-@camz).i)*@rotz).rect
      return if z <= 0
      [x/z*2, y/z*2, z, p[3] || 1]  # p[3]がnilの場合にデフォルト値1を設定
    }
    x0,x1=[a[0],b[0],c[0]].minmax
    ([width*(1+x0)*0.5,0].max.ceil..[width*(1+x1)*0.5,width-1].min).each{|ix|
      x=ix*2.0/width-1
      ax=a[0]-x
      bx=b[0]-x
      cx=c[0]-x
      y0,y1=[
        ax*bx <= 0 ? (ax*b[1] - bx*a[1]).fdiv(ax - bx) : nil,
        bx*cx <= 0 ? (bx*c[1] - cx*b[1]).fdiv(bx - cx) : nil,
        cx*ax <= 0 ? (cx*a[1] - ax*c[1]).fdiv(cx - ax) : nil
      ].compact.reject(&:nan?).minmax
      next if y0.nil?
      vax = a[0]*a[2]
      vay = a[1]*a[2]
      vaz = a[2]
      vabx = b[0]*b[2] - vax
      vaby = b[1]*b[2] - vay
      vabz = b[2] - a[2]
      vacx = c[0]*c[2] - vax
      vacy = c[1]*c[2] - vay
      vacz = c[2] - a[2]
      ([((y0*width+height)*0.5).ceil,0].max..[((y1*width+height)*0.5).floor,height-1].min).each{|iy|
        y=(iy*2.0-height)/width
        sx = vaby - vabz * y
        sy = vabz * x - vabx
        tx = vacy - vacz * y
        ty = vacz * x - vacx
        d = sx * ty - sy * tx
        vx = vaz * y - vay
        vy = vax - vaz * x
        s = (vx * ty - vy * tx).fdiv d
        t = (vy * sx - vx * sy).fdiv d
        z = vaz + vabz * s + vacz * t
        if z < depth[iy][ix]
          depth[iy][ix] = z
          color[iy][ix] = a[3] + (b[3] - a[3]) * s + (c[3] - a[3]) * t
        end
      }
    }
  end
  def show
    $><< "\e[1;1H"+color.each_slice(2).map{|a,b|a.zip(b).map{|a,b|TABLE[a*15.9][b*15.9]}*''}*$/
  end
end


def inverted_triangle(scale_factor)
  base_size = 0.1 * scale_factor  # スケールファクターを適用
  height = 0.2 * scale_factor  # スケールファクターを適用
  y_offset = 0.9  # 逆三角形の位置を上に移動するオフセット
  
  # 頂点の定義 (ベースの2つの頂点と下部の頂点)
  vertices = [
    [-base_size, 0, y_offset],
    [base_size, 0, y_offset],
    [0, height, y_offset]  # 上の頂点
  ]
  
  # 三角形の定義 (各三角形は3つの頂点で定義)
  triangles = [
    [vertices[0], vertices[1], vertices[2]]
  ]
  
  triangles
end

renderfont=->c,t0{
  c.camera(0,0,0,Math::PI/2,-Math::PI/2)
  %w[CHURADATA RUBY].each_with_index{|s,yi|
    s.chars.each_with_index{|ch,xi|
      t = [2+(xi+4*yi)*0.04-t0,0].max
      next if t>1
      t*=t
      vx,vy,vz,r1,r2,r3=6.times.map{|k|Math.sin(123*~k*~xi*~yi)*4%1}
      FONT[ch.to_sym].each{|tri|
        c.triangle *tri.map{|fx,fy|
          x=(fx-5)/120.0*(1-t)
          y=(7-fy)/120.0*(1-t)
          z=0
          x0=-0.4+xi*0.1
          y0=0.2-0.4*yi
          2.times{
            x,y=(x+y.i).*(32**(t*r1).i).rect
            y,z=(y+z.i).*(32**(t*r2).i).rect
            z,x=(z+x.i).*(32**(t*r3).i).rect
          }
          [x0+vx*t/2+x,y0+vy*t/2+y,-1+vz*t/2+z,1]
        }
      }
    }
  }
}

c=Canvas.new(W,H)
cnt=0


loop {
  time0 = Time.now
  cnt += 1
  t = cnt * 0.01

  # 逆三角形のスケーリングファクターを計算
  scale_factor = 1
  if t < 2
    scale_factor = 12 - t * 6  # 初めの3秒間でスケールを5から1に縮小
  elsif t >= 1
    scale_factor = 0
  end

  # カメラの設定
  th = Math::PI/2 + 0.8
  cam = [-1.2 * Math.cos(th), -1.2 * Math.sin(th), 1.5, th, -0.5]
  c.camera(*cam)
  c.clear(0.4, t * 10, 0.4, 0.6)

  # 逆三角形を描画
  inverted_triangle(scale_factor).each do |tri|
    c.triangle(*tri)
  end

  # 文字の描画 (必要に応じて)
  renderfont[c, t]

  # 描画の表示
  c.show
  sleep [0.05 - (Time.now - time0), 0.01].max
}