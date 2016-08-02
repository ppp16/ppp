package javaCode;
 
import com.jogamp.newt.opengl.GLWindow;
import com.jogamp.opengl.GL2;
import com.jogamp.opengl.GLAutoDrawable;
import com.jogamp.opengl.GLCapabilities;
import com.jogamp.opengl.GLEventListener;
import com.jogamp.opengl.GLProfile;
import com.jogamp.opengl.glu.GLU;
import com.jogamp.opengl.glu.GLUquadric;
import com.jogamp.opengl.util.FPSAnimator;


public class StartClient2 implements GLEventListener {

	// settings

	private static int FPS = 120;
	private int WIDTH = 800;
	private int HEIGHT = 600;

	// basic init values
	private GLU glu = new GLU();
	private GL2 gl;
	private static GLWindow window = null;

	
	private Vert ball = null;
	

//	public static void main(String[] args) {
//		new StartClient2();
//		
//	}
	public StartClient2(){
		GLProfile profile = GLProfile.get(GLProfile.GL2);
		GLCapabilities capabilities = new GLCapabilities(profile);

		// sets of the window and starts everything
		window = GLWindow.create(capabilities);
		window.setSize(WIDTH, HEIGHT);
		window.setVisible(true);
		window.setTitle("Crazy Maze");
		window.setAnimator(new FPSAnimator(window, FPS));
		window.getAnimator().start();// <--- creates background thread to run
										// the display method in a loop
		window.addGLEventListener(this);		
	}

	
	
	public void init(GLAutoDrawable drawable) {
		GL2 gl = drawable.getGL().getGL2();
		this.gl = gl;
		gl.glEnable(GL2.GL_DEPTH_TEST);
		gl.glEnable(GL2.GL_TEXTURE_2D);
		gl.glTexParameteri(GL2.GL_TEXTURE_2D, GL2.GL_TEXTURE_WRAP_S,
				GL2.GL_CLAMP);
		
		float[] color = {1,0,0,0};
		ball = new Vert(gl, glu, new Point3f(0,0,-6f), 0.1f, color, 1, true);

	}

	@Override
	public void dispose(GLAutoDrawable drawable) {
		System.exit(1);
	}

	@Override
	public void display(GLAutoDrawable drawable) {
		GL2 gl = drawable.getGL().getGL2();
		gl.glClear(GL2.GL_COLOR_BUFFER_BIT | GL2.GL_DEPTH_BUFFER_BIT);
		ball.draw();
		gl.glFlush();
	}

	@Override
	public void reshape(GLAutoDrawable drawable, int x, int y, int width,
			int height) {
		// reshape method to keep content of the window scaled right
		// comment in the 3 commented lines for no stretching but keeping the 1:1 ratio
		if (height < 1)
			height = 1;
		float h = (float) width / (float) height;
		gl.glViewport(0, 0, width, height);
		gl.glMatrixMode(GL2.GL_PROJECTION);
		gl.glLoadIdentity();
		glu.gluPerspective(45.0f, 1, 1, 20.0);
//		glu.gluPerspective(45.0f, h, 1, 20.0);
		gl.glMatrixMode(GL2.GL_MODELVIEW);
		gl.glLoadIdentity();
//		WIDTH = width;
//		HEIGHT = height;
//		}
	}
	
	public void translate(float x){
		ball.translate(x, x, x);
	}
	
	private class Vert {

		float radius = 0;
		private Point3f center;
		private float[] color;
		private boolean fill;

		public Vert(GL2 gl, GLU glu, Point3f center,
				float radius, float[] color, int id, boolean fill
				) {
			this.center = center;
			this.radius = radius;
			this.color = color;
			this.fill = fill;
		}

		public void translate(float x,float y,  float z){
			center.x += x;
			center.y += y;
			center.z += z;
		}

		public void draw() {

			gl.glPushMatrix();
			if (color != null)
				gl.glColor4f(color[0], color[1], color[2], color[3]);
			if (fill)
				gl.glPolygonMode(GL2.GL_FRONT_AND_BACK, GL2.GL_FILL);
			else
				gl.glPolygonMode(GL2.GL_FRONT_AND_BACK, GL2.GL_LINE);
			// gl.glLoadIdentity();
			if (center != null)
				gl.glTranslatef(center.x, center.y, center.z);


	        GLUquadric sphere = glu.gluNewQuadric();
	        glu.gluQuadricDrawStyle(sphere, GLU.GLU_FILL);
	        glu.gluQuadricNormals(sphere, GLU.GLU_FLAT);
	        glu.gluQuadricOrientation(sphere, GLU.GLU_OUTSIDE);
	        final int slices = 16;
	        final int stacks = 16;
	        glu.gluSphere(sphere, radius, slices, stacks);
	        glu.gluDeleteQuadric(sphere);
			// resets to white
			if (color != null)
				gl.glColor4f(1f,1f,1f, 0f);
			gl.glPopMatrix();
	 
		}
	


	}
	
	private class Point3f {

	public float x;
	public float y;
	public float z;
	
	public Point3f(float x, float y, float z){
		this.x = x;
		this.y = y;
		this.z = z;
	}
	
	public Point3f(float[] pos){
		this.x = pos[0];
		this.y = pos[1];
		this.z = pos[2];
	}

	public void translate(float x, float y, float z) {
		this.x+=x;
		this.y+=y;
		this.z+=z;
	}
	
	
	public void rotate(float x, float y, float z){
		x = (float) (x*Math.PI/180);
		y = (float) (y*Math.PI/180);
		z = (float) (z*Math.PI/180);
		if(x!=0){
			float orgY = this.y;float orgZ = this.z;
			this.y = (float) (orgY*Math.cos(x)) + orgZ*(float) -Math.sin(x);
			this.z = (float) (orgY*Math.sin(x)) + orgZ*(float) Math.cos(x);
		}
		if(y!=0){
			float orgX = this.x;float orgZ = this.z;
			this.x = (float) (orgX*Math.cos(y)) + orgZ*(float) Math.sin(y);
			this.z = (float) (-orgX*Math.sin(y)) + orgZ*(float) Math.cos(y);
		}
		if(z!=0){
			float orgX = this.x;float orgY = this.y;
			this.x = (float) (orgX*Math.cos(z)) + orgY*(float) -Math.sin(z);
			this.y = (float) (orgX*Math.sin(z)) + orgY*(float) Math.cos(z);
		}
	}
	
	public void scale(float x, float y, float z){
		this.x = x*this.x;
		this.y = y*this.y;
		this.z = z*this.z;
	}

}

}
