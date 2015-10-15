package com.github.macpersia.planty_jira_view;

import org.apache.commons.httpclient.params.HttpConnectionParams;
import org.apache.commons.httpclient.protocol.ProtocolSocketFactory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;

/**
 * A workaround for https://forums.openshift.com/commons-httpclient-permission-denied.
 */
public class OSProtocolSocketFactory implements ProtocolSocketFactory {

    private static final Log LOG = LogFactory.getLog(OSProtocolSocketFactory.class);

    @Override
    public Socket createSocket(String host, int port,
                               InetAddress localAddress, int localPort)
            throws IOException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("createSocket called. host = " + host + ", port = " + port
                    + ", ignoring localAddress = " + ((localAddress != null) ? localAddress.toString() : "null")
                    + ", ignoring localPort = " + localPort);
        }
        Socket socket = null;
        try {
            socket = new Socket(host, port);
            LOG.debug("Socket created");

        } catch (IOException e) {
            LOG.error("Error creating socket: " + e.getMessage());
            throw e;
        }
        return socket;
    }

    @Override
    public Socket createSocket(String host, int port, InetAddress localAddress,
                               int localPort, HttpConnectionParams params)
            throws IOException {

        LOG.debug("createSocket called with HttpConnectionParams -- ignoring the timeout value and proceeding");
        return this.createSocket(host, port, localAddress, localPort);
    }

    @Override
    public Socket createSocket(String host, int port)
            throws IOException {

        LOG.debug("createSocket called with just host and port. proceeding..");
        return this.createSocket(host, port, null, 0);
    }
}